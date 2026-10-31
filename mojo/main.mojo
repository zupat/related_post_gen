# Mojo's standard library has no JSON package, so this implementation carries a
# small parser for the benchmark input. The hot loop groups posts by their
# three-tag combinations: a candidate sharing s tags appears in exactly C(s, 3)
# groups, so ranking by that count gives the same order as ranking by s while
# touching far fewer entries than counting every co-tagged post. If fewer than
# five posts share three tags, the fallback counts every shared tag exactly.
from std.time import monotonic
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


def fill_top(
    ref counts: List[UInt32],
    ref source: List[Int],
    self_idx: Int,
    mut best_v: List[Int],
    mut best_i: List[Int],
) -> Int:
    var filled = 0
    var cutoff = -1
    for tk in range(len(source)):
        var j = source[tk]
        if j == self_idx:
            continue
        var c = Int(counts[j])
        if filled < 5 or c > cutoff:
            var pos = filled
            if pos > 4:
                pos = 4
            while pos > 0 and best_v[pos - 1] < c:
                best_v[pos] = best_v[pos - 1]
                best_i[pos] = best_i[pos - 1]
                pos -= 1
            best_v[pos] = c
            best_i[pos] = j
            if filled < 5:
                filled += 1
            cutoff = best_v[4]
    return filled


def main() raises:
    var f = open("../posts.json", "r")
    var raw = f.read()
    f.close()

    var data = List[UInt8](raw.as_bytes())
    var parser = JsonParser(data^)
    var posts = parser.parse_posts()
    var size = len(posts)

    # The timed region starts after parsing, like the other implementations:
    # building the tag index and the three-tag groups is part of the work.
    var start = monotonic()

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

    # Sorted ids make the three-tag key unambiguous.
    for i in range(size):
        ref ids = post_tag_ids[i]
        for a in range(1, len(ids)):
            var v = ids[a]
            var b = a - 1
            while b >= 0 and ids[b] > v:
                ids[b + 1] = ids[b]
                b -= 1
            ids[b + 1] = v

    var starts = List[Int](length=len(tag_freq) + 1, fill=0)
    var total_indices = 0
    for t in range(len(tag_freq)):
        starts[t] = total_indices
        total_indices += tag_freq[t]
    starts[len(tag_freq)] = total_indices

    var tag_indices = List[Int](length=total_indices, fill=0)
    var cursor = starts.copy()
    for i in range(size):
        ref ids = post_tag_ids[i]
        for t in range(len(ids)):
            var tid = ids[t]
            tag_indices[cursor[tid]] = i
            cursor[tid] += 1

    var num_tags = len(tag_freq)
    # The related score is |tags(i) & tags(j)|. A candidate sharing s tags
    # appears in exactly C(s,3) of the post's three-tag groups, so ranking by
    # that count is the same as ranking by s. This avoids counting every
    # co-tagged post when the top five all share at least three tags.
    var triple_key_to_group = Dict[Int, Int]()
    var triple_lists = List[List[Int]]()
    for i in range(size):
        ref ids = post_tag_ids[i]
        for x in range(len(ids)):
            var a = ids[x]
            for y in range(x + 1, len(ids)):
                var b = ids[y]
                for z in range(y + 1, len(ids)):
                    var c = ids[z]
                    var key = (a * num_tags + b) * num_tags + c
                    if key not in triple_key_to_group:
                        triple_key_to_group[key] = len(triple_lists)
                        triple_lists.append(List[Int]())
                    triple_lists[triple_key_to_group[key]].append(i)

    var num_groups = len(triple_lists)
    var triple_offsets = List[Int](length=num_groups + 1, fill=0)
    var triple_total = 0
    for g in range(num_groups):
        triple_offsets[g] = triple_total
        triple_total += len(triple_lists[g])
    triple_offsets[num_groups] = triple_total
    var triple_indices = List[Int](length=triple_total, fill=0)
    var triple_cursor = triple_offsets.copy()
    for g in range(num_groups):
        ref plist = triple_lists[g]
        var at = triple_cursor[g]
        for k in range(len(plist)):
            triple_indices[at] = plist[k]
            at += 1

    var related = List[Int](length=size * 5, fill=-1)
    var best_v = List[Int](length=5, fill=-1)
    var best_i = List[Int](length=5, fill=-1)
    var triple_counts = List[UInt32](length=size, fill=0)
    var touched = List[Int](capacity=size)

    for i in range(size):
        touched.clear()
        ref ids = post_tag_ids[i]
        for x in range(len(ids)):
            var a = ids[x]
            for y in range(x + 1, len(ids)):
                var b = ids[y]
                for z in range(y + 1, len(ids)):
                    var c = ids[z]
                    var key = (a * num_tags + b) * num_tags + c
                    if key not in triple_key_to_group:
                        continue
                    var g = triple_key_to_group[key]
                    var lo = triple_offsets[g]
                    var hi = triple_offsets[g + 1]
                    for k in range(lo, hi):
                        var idx = triple_indices[k]
                        var count = triple_counts[idx] + 1
                        triple_counts[idx] = count
                        if count == 1:
                            touched.append(idx)

        for k in range(5):
            best_v[k] = -1
            best_i[k] = -1
        var filled = fill_top(triple_counts, touched, i, best_v, best_i)

        if filled < 5:
            # Rare fallback: not enough posts share at least three tags, so
            # count every shared tag exactly.
            var counts = List[UInt8](length=size, fill=0)
            for t in range(len(ids)):
                var tid = ids[t]
                var lo = starts[tid]
                var hi = starts[tid + 1]
                for k in range(lo, hi):
                    var idx = tag_indices[k]
                    counts[idx] += 1
            counts[i] = 0
            for k in range(5):
                best_v[k] = -1
                best_i[k] = -1
            var pos_filled = 0
            var cutoff = -1
            for j in range(size):
                if j == i:
                    continue
                var c = Int(counts[j])
                if pos_filled < 5 or c > cutoff:
                    var pos = pos_filled
                    if pos > 4:
                        pos = 4
                    while pos > 0 and best_v[pos - 1] < c:
                        best_v[pos] = best_v[pos - 1]
                        best_i[pos] = best_i[pos - 1]
                        pos -= 1
                    best_v[pos] = c
                    best_i[pos] = j
                    if pos_filled < 5:
                        pos_filled += 1
                    cutoff = best_v[4]

        for k in range(5):
            related[i * 5 + k] = best_i[k]

        for tk in range(len(touched)):
            triple_counts[touched[tk]] = 0

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
