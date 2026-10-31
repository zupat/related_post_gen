# Exact shared-tag counting: the same algorithm as go/rust/d. The tag -> post
# index is a CSR of UInt32 (4-byte indices) and top-5 selection mirrors go/d: one
# compare per candidate over fixed 5 slots, all through safe list indexing.
#
# AGENT.md forbids FFI, with one approved exception: the per-post counter reset
# calls libc memset. Go's equivalent reset loop compiles to
# runtime.memclrNoHeapPointers, so a byte-at-a-time loop would measure something
# the other implementations do not do. `unsafe_ptr()` below exists only to hand
# the buffer to memset; no other pointer or unsafe construct is used.
from std.time import monotonic
from std.ffi import external_call
from std.os import abort


@fieldwise_init
struct Post:
    var id: String
    var title: String
    var tags: List[String]


struct JsonParser:
    var data: List[UInt8]
    var pos: Int

    def __init__(out self, var data: List[UInt8]):
        self.data = data^
        self.pos = 0

    def skip_ws(mut self):
        while self.pos < len(self.data):
            var c = self.data[self.pos]
            if c == 32 or c == 10 or c == 13 or c == 9:
                self.pos += 1
            else:
                break

    def expect(mut self, expected: UInt8):
        self.skip_ws()
        if self.pos >= len(self.data) or self.data[self.pos] != expected:
            abort("invalid JSON")
        self.pos += 1

    def parse_hex4(mut self) -> Int:
        var value = 0
        for _ in range(4):
            var c = self.data[self.pos]
            var digit = 0
            if c >= 48 and c <= 57:
                digit = Int(c) - 48
            elif c >= 65 and c <= 70:
                digit = Int(c) - 55
            elif c >= 97 and c <= 102:
                digit = Int(c) - 87
            else:
                abort("invalid unicode escape")
            value = value * 16 + digit
            self.pos += 1
        return value

    def parse_string(mut self) -> String:
        self.skip_ws()
        if self.data[self.pos] != 34:
            abort("expected string")
        self.pos += 1
        var start = self.pos
        # Fast path: no escapes.
        while self.pos < len(self.data):
            var c = self.data[self.pos]
            if c == 34:
                var result = String(from_utf8_lossy=self.data[start : self.pos])
                self.pos += 1
                return result^
            if c == 92:
                break
            self.pos += 1

        var out = String(capacity_bytes=64)
        var chunk = start
        while self.pos < len(self.data):
            var c = self.data[self.pos]
            if c == 34:
                if self.pos > chunk:
                    out += String(from_utf8_lossy=self.data[chunk : self.pos])
                self.pos += 1
                return out^
            if c == 92:
                if self.pos > chunk:
                    out += String(from_utf8_lossy=self.data[chunk : self.pos])
                self.pos += 1
                if self.pos >= len(self.data):
                    abort("unterminated escape")
                var e = self.data[self.pos]
                self.pos += 1
                if e == 34:
                    out += '"'
                elif e == 92:
                    out += "\\"
                elif e == 47:
                    out += "/"
                elif e == 98:
                    out += "\b"
                elif e == 102:
                    out += "\f"
                elif e == 110:
                    out += "\n"
                elif e == 114:
                    out += "\r"
                elif e == 116:
                    out += "\t"
                elif e == 117:
                    var cp = self.parse_hex4()
                    out += chr(cp)
                else:
                    abort("invalid escape")
                chunk = self.pos
            else:
                self.pos += 1
        abort("unterminated string")

    def skip_value(mut self):
        self.skip_ws()
        var c = self.data[self.pos]
        if c == 34:
            _ = self.parse_string()
        elif c == 123:
            self.pos += 1
            self.skip_ws()
            if self.data[self.pos] == 125:
                self.pos += 1
                return
            while True:
                _ = self.parse_string()
                self.expect(58)
                self.skip_value()
                self.skip_ws()
                if self.data[self.pos] == 44:
                    self.pos += 1
                elif self.data[self.pos] == 125:
                    self.pos += 1
                    return
                else:
                    abort("invalid object")
        elif c == 91:
            self.pos += 1
            self.skip_ws()
            if self.data[self.pos] == 93:
                self.pos += 1
                return
            while True:
                self.skip_value()
                self.skip_ws()
                if self.data[self.pos] == 44:
                    self.pos += 1
                elif self.data[self.pos] == 93:
                    self.pos += 1
                    return
                else:
                    abort("invalid array")
        else:
            while self.pos < len(self.data):
                var d = self.data[self.pos]
                if (
                    d == 44
                    or d == 93
                    or d == 125
                    or d == 32
                    or d == 10
                    or d == 13
                    or d == 9
                ):
                    return
                self.pos += 1

    def parse_tags(mut self) -> List[String]:
        var tags = List[String]()
        self.skip_ws()
        if self.data[self.pos] != 91:
            abort("expected tags array")
        self.pos += 1
        self.skip_ws()
        if self.data[self.pos] == 93:
            self.pos += 1
            return tags^
        while True:
            tags.append(self.parse_string())
            self.skip_ws()
            if self.data[self.pos] == 44:
                self.pos += 1
            elif self.data[self.pos] == 93:
                self.pos += 1
                return tags^
            else:
                abort("invalid tags array")

    def parse_post(mut self) -> Post:
        var pid = String()
        var title = String()
        var tags = List[String]()
        self.skip_ws()
        if self.data[self.pos] != 123:
            abort("expected post object")
        self.pos += 1
        self.skip_ws()
        if self.data[self.pos] == 125:
            self.pos += 1
            return Post(pid^, title^, tags^)
        while True:
            var key = self.parse_string()
            self.expect(58)
            if key == "_id":
                pid = self.parse_string()
            elif key == "title":
                title = self.parse_string()
            elif key == "tags":
                tags = self.parse_tags()
            else:
                self.skip_value()
            self.skip_ws()
            if self.data[self.pos] == 44:
                self.pos += 1
            elif self.data[self.pos] == 125:
                self.pos += 1
                return Post(pid^, title^, tags^)
            else:
                abort("invalid post object")

    def parse_posts(mut self) -> List[Post]:
        var posts = List[Post]()
        self.skip_ws()
        if self.data[self.pos] != 91:
            abort("expected posts array")
        self.pos += 1
        self.skip_ws()
        if self.data[self.pos] == 93:
            self.pos += 1
            return posts^
        while True:
            posts.append(self.parse_post())
            self.skip_ws()
            if self.data[self.pos] == 44:
                self.pos += 1
            elif self.data[self.pos] == 93:
                self.pos += 1
                return posts^
            else:
                abort("invalid posts array")


def write_json_string(mut out: String, s: String):
    var b = s.as_bytes()
    out += '"'
    var start = 0
    for i in range(len(b)):
        var c = b[i]
        if c == 34 or c == 92 or c < 32:
            if i > start:
                out += String(from_utf8_lossy=b[start:i])
            if c == 34:
                out += '\\"'
            elif c == 92:
                out += "\\\\"
            elif c == 8:
                out += "\\b"
            elif c == 12:
                out += "\\f"
            elif c == 10:
                out += "\\n"
            elif c == 13:
                out += "\\r"
            elif c == 9:
                out += "\\t"
            else:
                out += "\\u00"
                var hex_digits = "0123456789abcdef"
                out += String(hex_digits[byte=(Int(c) >> 4) & 15])
                out += String(hex_digits[byte=Int(c) & 15])
            start = i + 1
    if start < len(b):
        out += String(from_utf8_lossy=b[start:])
    out += '"'


def write_post(mut out: String, post: Post):
    out += '{"_id":'
    write_json_string(out, post.id)
    out += ',"title":'
    write_json_string(out, post.title)
    out += ',"tags":['
    for t in range(len(post.tags)):
        if t > 0:
            out += ","
        write_json_string(out, post.tags[t])
    out += "]}"


def main() raises:
    var f = open("../posts.json", "r")
    var raw = f.read()
    f.close()

    var data = List[UInt8](raw.as_bytes())
    var parser = JsonParser(data^)
    var posts = parser.parse_posts()
    var size = len(posts)

    # Timed region starts after parsing, like the other implementations.
    var start = monotonic()

    # tag -> post indices, flattened into CSR arrays (same shape as mojo/)
    var tag_to_id = Dict[String, Int]()
    var tag_freq = List[Int]()
    var post_tag_ids = List[List[Int]]()
    for i in range(size):
        var ids = List[Int]()
        ref tags = posts[i].tags
        for t in range(len(tags)):
            var tag = tags[t]
            if tag not in tag_to_id:
                tag_to_id[tag] = len(tag_freq)
                tag_freq.append(0)
            var tid = tag_to_id[tag]
            # verify.py compares tag sets, so a repeated tag in one post must
            # not count twice.
            var duplicate = False
            for q in range(len(ids)):
                if ids[q] == tid:
                    duplicate = True
                    break
            if not duplicate:
                ids.append(tid)
                tag_freq[tid] += 1
        post_tag_ids.append(ids^)

    var starts = List[UInt32](length=len(tag_freq) + 1, fill=0)
    var total_indices = 0
    for t in range(len(tag_freq)):
        starts[t] = UInt32(total_indices)
        total_indices += tag_freq[t]
    starts[len(tag_freq)] = UInt32(total_indices)

    var tag_indices = List[UInt32](length=total_indices, fill=0)
    var cursor = List[Int](length=len(tag_freq), fill=0)
    for t in range(len(tag_freq)):
        cursor[t] = Int(starts[t])
    for i in range(size):
        ref ids = post_tag_ids[i]
        for t in range(len(ids)):
            var tid = ids[t]
            tag_indices[cursor[tid]] = UInt32(i)
            cursor[tid] += 1

    var counts = List[UInt8](length=size, fill=0)
    # Exception to the no-FFI rule (see header): memset needs the buffer address.
    var counts_ptr = counts.unsafe_ptr()
    var related = List[Int](length=size * 5, fill=-1)
    var best_v = List[Int](length=5, fill=0)
    var best_i = List[Int](length=5, fill=0)

    for i in range(size):
        ref ids = post_tag_ids[i]
        # Count every shared tag exactly (the go/rust/d hot loop).
        for t in range(len(ids)):
            var tid = ids[t]
            var lo = Int(starts[tid])
            var hi = Int(starts[tid + 1])
            for k in range(lo, hi):
                var idx = Int(tag_indices[k])
                counts[idx] = counts[idx] + 1
        counts[i] = 0  # don't recommend the same post

        # Same shape as go/d: one compare per candidate and a fixed top-5.
        # counts[i] is already 0, so the self post is never selected and no
        # per-element self check is needed; cutoff starts at 0.
        for k in range(5):
            best_v[k] = 0
            best_i[k] = 0
        var cutoff = 0
        for j in range(size):
            var c = Int(counts[j])
            if c > cutoff:
                best_v[4] = c
                best_i[4] = j
                var q = 3
                while q >= 0 and best_v[q] < c:
                    best_v[q + 1] = best_v[q]
                    best_i[q + 1] = best_i[q]
                    q -= 1
                best_v[q + 1] = c
                best_i[q + 1] = j
                cutoff = best_v[4]

        for k in range(5):
            related[i * 5 + k] = best_i[k]

        # Clear the counters for the next post, like go's resetSlice loop, which
        # the Go compiler lowers to runtime.memclrNoHeapPointers.
        _ = external_call["memset", NoneType](counts_ptr, 0, size)

    var end = monotonic()
    var elapsed_ms = Float64(end - start) / 1_000_000.0
    print("Processing time (w/o IO):", elapsed_ms, "ms")

    var out = String(capacity_bytes=64 * 1024 * 1024)
    out += "["
    for i in range(size):
        if i > 0:
            out += ","
        ref post = posts[i]
        out += '{"_id":'
        write_json_string(out, post.id)
        out += ',"title":'
        write_json_string(out, post.title)
        out += ',"tags":['
        for t in range(len(post.tags)):
            if t > 0:
                out += ","
            write_json_string(out, post.tags[t])
        out += '],"related":['
        for k in range(5):
            if k > 0:
                out += ","
            var idx = related[i * 5 + k]
            if idx >= 0:
                write_post(out, posts[idx])
        out += "]}"
    out += "]"

    var wf = open("../related_posts_mojo.json", "w")
    wf.write(out)
    wf.close()
