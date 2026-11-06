// Related posts benchmark for Goose.
//
// Parses posts.json with the standard-library JSON reader, interns every tag,
// counts shared tags per post, and writes the five most related posts for
// every post. All text is kept as offset/length pairs into one byte buffer,
// so the post array stays fixed-size and can be indexed.
import std;
import json;
import os;

const TOP_N = 5;

// Fixed-size so Post[>..] is indexable (arrays of variable-size elements are
// sequential). id/title are slices of `names`.
struct Post {
    id_off: i64,
    id_len: i64,
    title_off: i64,
    title_len: i64,
    tags_off: i64,
    tags_len: i64,
}

// Returns the id of `s`, appending it to the tag table when it is new.
fn intern(
    names: u8[>..]&,
    offs: i64[>..]&,
    lens: i64[>..]&,
    s: u8[:],
) -> u16 {
    for i in offs.len {
        let o = offs[i];
        let l = lens[i];
        if l == s.len && names[o..o + l] == s { return i as u16; }
    }
    offs.push(names.len);
    lens.push(s.len);
    names.append(s);
    return (offs.len - 1) as u16;
}

fn read_posts(
    text: u8[:],
    posts: Post[>..]&,
    post_tags: u16[>..]&,
    names: u8[>..]&,
    tag_off: i64[>..]&,
    tag_len: i64[>..]&,
) {
    var r = json::reader(text);
    var scratch: u8[>..] = [];
    json::each_element(r) {
        var p = Post {
            id_off: 0, id_len: 0,
            title_off: 0, title_len: 0,
            tags_off: post_tags.len, tags_len: 0,
        };
        json::each_member(r) { key =>
            if key == "_id" {
                let s = json::read_string(r, scratch);
                p.id_off = names.len;
                p.id_len = s.len;
                names.append(s);
            } else if key == "title" {
                let s = json::read_string(r, scratch);
                p.title_off = names.len;
                p.title_len = s.len;
                names.append(s);
            } else if key == "tags" {
                json::each_element(r) {
                    let s = json::read_string(r, scratch);
                    post_tags.push(intern(names, tag_off, tag_len, s));
                    p.tags_len += 1;
                };
            } else {
                json::skip(r);
            }
        };
        posts.push(p);
    };
    if !json::finish(r) { abort(json::message(r)); }
}

// Compressed sparse row layout: tag_start[t]..tag_start[t + 1] indexes the
// posts that carry tag t in by_tag.
fn build_tag_posts(
    posts: Post[:],
    post_tags: const u16[:],
    tag_count: i64,
    tag_start: i64[>..]&,
    by_tag: u32[>..]&,
) {
    var tag_counts: i64[>..] = [];
    for t in tag_count { tag_counts.push(0); }

    for i in posts.len {
        let p = posts[i];
        for k in p.tags_len {
            let tag = post_tags[p.tags_off + k] as i64;
            tag_counts[tag] += 1;
        }
    }

    var total = 0;
    for t in tag_count {
        tag_start.push(total);
        total += tag_counts[t];
    }
    tag_start.push(total);

    var cursors: i64[>..] = [];
    for t in tag_count { cursors.push(tag_start[t]); }
    for i in posts.len {
        let p = posts[i];
        for k in p.tags_len {
            let tag = post_tags[p.tags_off + k] as i64;
            by_tag[cursors[tag]] = i as u32;
            cursors[tag] += 1;
        }
    }
}

// Go's top-N selection: scan every post's shared-tag count and keep the five
// largest in a small insertion-sorted array.
fn top5(
    posts: Post[:],
    by_tag: const u32[:],
    tag_start: const i64[:],
    post_tags: const u16[:],
) -> u32[>..] {
    let n = posts.len;
    var related: u32[>..] = [];
    for i in n * TOP_N { related.push(0); }

    var counts: u8[>..] = [];
    for i in n { counts.push(0); }

    var top5: i64[TOP_N * 2] = [0; TOP_N * 2];

    for i in n {
        for j in n { counts[j] = 0; }

        let p = posts[i];
        for k in p.tags_len {
            let tag = post_tags[p.tags_off + k] as i64;
            let s = tag_start[tag];
            let e = tag_start[tag + 1];
            for c in s..e { counts[by_tag[c] as i64] += 1; }
        }
        counts[i] = 0;

        for k in TOP_N * 2 { top5[k] = 0; }
        var min_tags: u8 = 0;

        for j in n {
            let count = counts[j];
            if count <= min_tags { continue; }
            var upper: i64 = (TOP_N - 2) * 2;
            while upper >= 0 && count as i64 > top5[upper] {
                top5[upper + 2] = top5[upper];
                top5[upper + 3] = top5[upper + 1];
                upper -= 2;
            }
            let pos = upper + 2;
            top5[pos] = count as i64;
            top5[pos + 1] = j;
            min_tags = top5[TOP_N * 2 - 2] as u8;
        }

        for k in TOP_N {
            related[i * TOP_N + k] = top5[k * 2 + 1] as u32;
        }
    }
    return related;
}

fn write_string(out: u8[>..]&, names: const u8[:], off: i64, len: i64) {
    json::format_string(out, names[off..off + len]);
}

fn write_post(
    out: u8[>..]&,
    p: Post,
    names: const u8[:],
    post_tags: const u16[:],
    tag_off: const i64[:],
    tag_len: const i64[:],
) {
    out.append("{\"_id\":");
    write_string(out, names, p.id_off, p.id_len);
    out.append(",\"title\":");
    write_string(out, names, p.title_off, p.title_len);
    out.append(",\"tags\":[");
    for k in p.tags_len {
        if k > 0 { out.push(','); }
        let tag = post_tags[p.tags_off + k] as i64;
        write_string(out, names, tag_off[tag], tag_len[tag]);
    }
    out.append("]}");
}

fn write_output(
    posts: Post[:],
    related: const u32[:],
    names: const u8[:],
    post_tags: const u16[:],
    tag_off: const i64[:],
    tag_len: const i64[:],
) {
    var out: u8[>..] = [];
    out.append("[\n");
    for i in posts.len {
        if i > 0 { out.append(",\n"); }
        let p = posts[i];
        out.append("{\"_id\":");
        write_string(out, names, p.id_off, p.id_len);
        out.append(",\"tags\":[");
        for k in p.tags_len {
            if k > 0 { out.push(','); }
            let tag = post_tags[p.tags_off + k] as i64;
            write_string(out, names, tag_off[tag], tag_len[tag]);
        }
        out.append("],\"related\":[");
        for k in TOP_N {
            if k > 0 { out.push(','); }
            write_post(out, posts[related[i * TOP_N + k] as i64],
                names, post_tags, tag_off, tag_len);
        }
        out.append("]}");
    }
    out.append("\n]\n");
    if !write_file("../related_posts_goose.json", out) {
        abort("failed to write ../related_posts_goose.json");
    }
}

fn main() {
    var text: u8[>..] = [];
    if !read_file("../posts.json", text) {
        abort("failed to read ../posts.json");
    }

    var posts: Post[>..] = [];
    var post_tags: u16[>..] = [];
    var names: u8[>..] = [];
    var tag_off: i64[>..] = [];
    var tag_len: i64[>..] = [];
    read_posts(text, posts, post_tags, names, tag_off, tag_len);

    let start = clock_ns();
    var tag_start: i64[>..] = [];
    var by_tag: u32[>..] = [];
    for i in post_tags.len { by_tag.push(0); }
    build_tag_posts(posts, post_tags, tag_off.len, tag_start, by_tag);
    let related = top5(posts, by_tag, tag_start, post_tags);
    let elapsed_ns = clock_ns() - start;

    var msg: u8[>..] = [];
    msg.append("Processing time (w/o IO): ");
    format_flt(msg, elapsed_ns as f64 / 1000000.0, 3);
    msg.append("ms");
    print(msg);

    write_output(posts, related, names, post_tags, tag_off, tag_len);
}
