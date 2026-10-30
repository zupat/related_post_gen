const std = @import("std");
const Post = struct { _id: []const u8, title: []const u8, tags: [][]const u8 };
const Posts = []Post;
const TopPosts = struct { _id: *const []const u8, tags: *const [][]const u8, related: []*Post };
const fxhash = @import("fxhash.zig");

const Score = struct {
    s: u8,
    pos: usize,
};

// This is an explicit vector reduction because of an upstream LLVM 22
// regression: Zig 0.15.1 (LLVM 20.1.2) vectorized the byte reduction below into
// a 32-byte AVX2 `vpminub`/`vpcmpeqb` loop, but Zig 0.17.0 (LLVM 22.1.8) emits
// an 8x unrolled scalar `cmpb/setae/or` loop instead (`llvm.loop.isvectorized`
// is gone from the IR). The scalar loop scans ~1 byte/cycle instead of ~128
// bytes/iteration, which made the 60k run ~40% slower (1.61s -> 2.27s on a b3-8
// test VM). Rewriting it this way restores the old speed and produces
// byte-identical output. Keep this until LLVM vectorizes the idiom again.
inline fn is_top(m: u8, score: []const u8) u8 {
    const V = @Vector(32, u8);
    const mv: V = @splat(m);
    var any = false;
    var i: usize = 0;
    while (i + 32 <= score.len) : (i += 32) {
        const v: V = score[i..][0..32].*;
        any = any or @reduce(.Or, v > mv);
    }
    while (i < score.len) : (i += 1) {
        any = any or score[i] > m;
    }
    return @intFromBool(any);
}

inline fn get_top(b: usize, score: []const u8, min: *u8, t5: []Score) void {
    for (score, b..) |s, i| {
        if (s > min.*) {
            var u: i8 = 3;
            while (u >= 0 and s > t5[@intCast(u)].s) : (u -= 1) {
                t5[@intCast(u + 1)] = t5[@intCast(u)];
            }
            t5[@intCast(u + 1)] = Score{ .s = s, .pos = i };
            min.* = t5[4].s;
        }
    }
}

inline fn top5(related: []*Post, score: []u8, ps: []Post) void {
    const s = Score{ .s = 0, .pos = 0 };
    var t5 = [5]Score{ s, s, s, s, s };
    var min_tags: u8 = 0;

    var b: usize = 0;
    const cache_line: usize = 64;
    while (b < score.len) : (b += cache_line) {
        const e = @min(b + cache_line, score.len);
        const chunk = score[b..e];
        if (is_top(min_tags, chunk) > 0) {
            get_top(b, chunk, &min_tags, t5[0..]);
        }
    }
    for (t5, 0..) |t, i| {
        related[i] = &ps[t.pos];
    }
}

pub fn main() !void {
    var threaded: std.Io.Threaded = .init(std.heap.page_allocator, .{});
    defer threaded.deinit();
    const io = threaded.io();

    var allocator = std.heap.page_allocator;

    const file = try std.Io.Dir.cwd().openFile(io, "../posts.json", .{});
    defer file.close(io);
    var file_buffer: [4096]u8 = undefined;
    var file_reader = file.reader(io, &file_buffer);
    const io_reader = &file_reader.interface;
    var json_reader = std.json.Reader.init(allocator, io_reader);
    defer json_reader.deinit();
    const parsed = try std.json.parseFromTokenSource(Posts, allocator, &json_reader, .{});
    defer parsed.deinit();

    const start = std.Io.Clock.awake.now(io);

    const ArrPosts = std.ArrayList(u32);
    var map = fxhash.StringHashMap(ArrPosts).init(allocator);
    defer map.deinit();

    for (parsed.value, 0..) |post_ele, i| {
        for (post_ele.tags) |tag| {
            const get_or_put = try map.getOrPut(tag);
            if (get_or_put.found_existing) {
                try get_or_put.value_ptr.*.append(allocator, @intCast(i));
            } else {
                var temp = ArrPosts.empty;
                try temp.append(allocator, @intCast(i));
                get_or_put.value_ptr.* = temp;
            }
        }
    }

    var op = try std.ArrayList(TopPosts).initCapacity(allocator, parsed.value.len);
    op.expandToCapacity();
    defer op.deinit(allocator);

    var rl: []*Post = try allocator.alloc(*Post, parsed.value.len * 5);
    defer allocator.free(rl);

    var tagged_post_count: []u8 = try allocator.alloc(u8, parsed.value.len);
    defer allocator.free(tagged_post_count);

    for (0..parsed.value.len) |post_index| {
        // reset tagged_post_count
        @memset(tagged_post_count, 0);

        for (parsed.value[post_index].tags) |tag| {
            for (map.get(tag).?.items) |i_t| {
                tagged_post_count[i_t] += 1;
            }
        }

        tagged_post_count[post_index] = 0; // Don't count self

        const related: []*Post = rl[post_index * 5 .. post_index * 5 + 5];
        top5(related, tagged_post_count, parsed.value);
        op.items[post_index] = .{ ._id = &parsed.value[post_index]._id, .tags = &parsed.value[post_index].tags, .related = related };
    }
    const end = std.Io.Clock.awake.now(io);

    var stdout_buffer: [4096]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(io, &stdout_buffer);

    try stdout.interface.print("Processing time (w/o IO): {d}ms\n", .{@divFloor(start.durationTo(end).nanoseconds, std.time.ns_per_ms)});
    try stdout.interface.flush();

    const op_file = try std.Io.Dir.cwd().createFile(io, "../related_posts_zig.json", .{});
    defer op_file.close(io);

    var buf: [4096]u8 = undefined;
    var writer = op_file.writer(io, &buf);

    try std.json.Stringify.value(
        try op.toOwnedSlice(allocator),
        .{},
        &writer.interface,
    );

    try writer.interface.flush();
}
