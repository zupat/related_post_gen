// Related posts benchmark for Hare.
//
// Parses posts.json, maps each tag to the posts carrying it, and writes the
// five posts with the most shared tags for every post.
use bufio;
use bytes;
use fmt;
use io;
use os;
use strings;
use time;

type post = struct {
	id: []u8,
	title: []u8,
	tags: []u16,
};

type parser = struct {
	buf: []u8,
	pos: size,
};

type tag_table = struct {
	heads: []i32,
	next: []i32,
	tags: [][]u8,
	counts: []u32,
};

const TAG_BUCKETS: size = 512;

fn hash_bytes(s: []u8) u32 = {
	let h: u32 = 2166136261;
	for (let i = 0z; i < len(s); i += 1) {
		h ^= s[i]: u32;
		h *= 16777619;
	};
	return h;
};

fn skip_ws(p: *parser) void = {
	for (p.pos < len(p.buf)) {
		const c = p.buf[p.pos];
		if (c != ' ' && c != '\t' && c != '\n' && c != '\r') {
			break;
		};
		p.pos += 1;
	};
};

fn peek(p: *parser) u8 = {
	if (p.pos >= len(p.buf)) {
		abort("unexpected end of JSON input");
	};
	return p.buf[p.pos];
};

fn expect(p: *parser, c: u8) void = {
	skip_ws(p);
	if (peek(p) != c) {
		abort("invalid JSON input");
	};
	p.pos += 1;
};

fn hex_digit(c: u8) u32 = {
	if (c >= '0' && c <= '9') {
		return c: u32 - '0': u32;
	} else if (c >= 'a' && c <= 'f') {
		return c: u32 - 'a': u32 + 10;
	} else if (c >= 'A' && c <= 'F') {
		return c: u32 - 'A': u32 + 10;
	};
	abort("invalid escape in JSON string");
};

fn parse_hex4(s: []u8) u32 = {
	return hex_digit(s[0]) << 12 | hex_digit(s[1]) << 8
		| hex_digit(s[2]) << 4 | hex_digit(s[3]);
};

fn append_rune(out: []u8, cp: u32) []u8 = {
	if (cp < 0x80) {
		append(out, cp: u8)!;
	} else if (cp < 0x800) {
		append(out, (0xC0 | cp >> 6): u8)!;
		append(out, (0x80 | cp & 0x3F): u8)!;
	} else if (cp < 0x10000) {
		append(out, (0xE0 | cp >> 12): u8)!;
		append(out, (0x80 | cp >> 6 & 0x3F): u8)!;
		append(out, (0x80 | cp & 0x3F): u8)!;
	} else {
		append(out, (0xF0 | cp >> 18): u8)!;
		append(out, (0x80 | cp >> 12 & 0x3F): u8)!;
		append(out, (0x80 | cp >> 6 & 0x3F): u8)!;
		append(out, (0x80 | cp & 0x3F): u8)!;
	};
	return out;
};

fn unescape(s: []u8) []u8 = {
	let out: []u8 = alloc([], len(s))!;
	let i = 0z;
	for (i < len(s)) {
		const c = s[i];
		if (c != '\\') {
			append(out, c)!;
			i += 1;
			continue;
		};
		i += 1;
		if (i >= len(s)) {
			abort("invalid escape in JSON string");
		};
		const e = s[i];
		i += 1;
		if (e == '"' || e == '\\' || e == '/') {
			append(out, e)!;
		} else if (e == 'b') {
			append(out, 8)!;
		} else if (e == 'f') {
			append(out, 12)!;
		} else if (e == 'n') {
			append(out, 10)!;
		} else if (e == 'r') {
			append(out, 13)!;
		} else if (e == 't') {
			append(out, 9)!;
		} else if (e == 'u') {
			if (i + 4 > len(s)) {
				abort("invalid unicode escape in JSON string");
			};
			let cp = parse_hex4(s[i..i + 4]);
			i += 4;
			if (cp >= 0xD800 && cp <= 0xDBFF) {
				if (i + 6 > len(s) || s[i] != '\\' || s[i + 1] != 'u') {
					abort("invalid surrogate pair in JSON string");
				};
				const low = parse_hex4(s[i + 2..i + 6]);
				if (low < 0xDC00 || low > 0xDFFF) {
					abort("invalid surrogate pair in JSON string");
				};
				i += 6;
				cp = 0x10000 + ((cp - 0xD800) << 10)
					+ (low - 0xDC00);
			} else if (cp >= 0xDC00 && cp <= 0xDFFF) {
				abort("invalid surrogate pair in JSON string");
			};
			out = append_rune(out, cp);
		} else {
			abort("invalid escape in JSON string");
		};
	};
	return out;
};

fn parse_string(p: *parser) []u8 = {
	skip_ws(p);
	if (peek(p) != '"') {
		abort("expected string in JSON input");
	};
	p.pos += 1;
	const start = p.pos;
	let escaped = false;
	for (p.pos < len(p.buf)) {
		const c = p.buf[p.pos];
		if (c == '"') {
			const s = p.buf[start..p.pos];
			p.pos += 1;
			return if (escaped) unescape(s) else s;
		} else if (c == '\\') {
			escaped = true;
			p.pos += 2;
		} else {
			p.pos += 1;
		};
	};
	abort("unterminated string in JSON input");
};

fn skip_value(p: *parser) void = {
	skip_ws(p);
	const c = peek(p);
	if (c == '"') {
		_ = parse_string(p);
	} else if (c == '{') {
		p.pos += 1;
		for (true) {
			skip_ws(p);
			if (peek(p) == '}') {
				p.pos += 1;
				break;
			};
			_ = parse_string(p);
			expect(p, ':');
			skip_value(p);
			skip_ws(p);
			if (peek(p) == ',') {
				p.pos += 1;
			} else if (peek(p) == '}') {
				p.pos += 1;
				break;
			} else {
				abort("invalid object in JSON input");
			};
		};
	} else if (c == '[') {
		p.pos += 1;
		for (true) {
			skip_ws(p);
			if (peek(p) == ']') {
				p.pos += 1;
				break;
			};
			skip_value(p);
			skip_ws(p);
			if (peek(p) == ',') {
				p.pos += 1;
			} else if (peek(p) == ']') {
				p.pos += 1;
				break;
			} else {
				abort("invalid array in JSON input");
			};
		};
	} else {
		for (p.pos < len(p.buf)) {
			const b = p.buf[p.pos];
			if (b == ',' || b == '}' || b == ']' || b == ' '
					|| b == '\t' || b == '\n' || b == '\r') {
				break;
			};
			p.pos += 1;
		};
	};
};

fn new_tag_table() tag_table = {
	let heads: []i32 = alloc([0...], TAG_BUCKETS)!;
	for (let i = 0z; i < len(heads); i += 1) {
		heads[i] = -1;
	};
	return tag_table {
		heads = heads,
		next = [],
		tags = [],
		counts = [],
	};
};

fn intern_tag(tt: *tag_table, s: []u8) u16 = {
	if (len(tt.tags) >= 65535) {
		abort("too many unique tags");
	};
	const bucket = hash_bytes(s): size & (TAG_BUCKETS - 1);
	let idx = tt.heads[bucket];
	for (idx >= 0) {
		const i = idx: size;
		if (bytes::equal(tt.tags[i], s)) {
			return i: u16;
		};
		idx = tt.next[i];
	};
	const id = len(tt.tags): u16;
	append(tt.tags, s)!;
	append(tt.next, tt.heads[bucket])!;
	append(tt.counts, 0)!;
	tt.heads[bucket] = id: i32;
	return id;
};

fn parse_tags(p: *parser, tt: *tag_table) []u16 = {
	expect(p, '[');
	let tags: []u16 = alloc([], 8)!;
	for (true) {
		skip_ws(p);
		if (peek(p) == ']') {
			p.pos += 1;
			break;
		};
		const tag = intern_tag(tt, parse_string(p));
		tt.counts[tag: size] += 1;
		append(tags, tag)!;
		skip_ws(p);
		if (peek(p) == ',') {
			p.pos += 1;
		} else if (peek(p) == ']') {
			p.pos += 1;
			break;
		} else {
			abort("invalid tags array in JSON input");
		};
	};
	return tags;
};

fn key_is(key: []u8, name: str) bool = {
	return bytes::equal(key, strings::toutf8(name));
};

fn parse_post(p: *parser, tt: *tag_table) post = {
	expect(p, '{');
	let id: []u8 = [];
	let title: []u8 = [];
	let tags: []u16 = alloc([], 8)!;
	for (true) {
		skip_ws(p);
		if (peek(p) == '}') {
			p.pos += 1;
			break;
		};
		const key = parse_string(p);
		expect(p, ':');
		if (key_is(key, "_id")) {
			id = parse_string(p);
		} else if (key_is(key, "title")) {
			title = parse_string(p);
		} else if (key_is(key, "tags")) {
			tags = parse_tags(p, tt);
		} else {
			skip_value(p);
		};
		skip_ws(p);
		if (peek(p) == ',') {
			p.pos += 1;
		} else if (peek(p) == '}') {
			p.pos += 1;
			break;
		} else {
			abort("invalid post object in JSON input");
		};
	};
	return post {
		id = id,
		title = title,
		tags = tags,
	};
};

fn parse_posts(buf: []u8, tt: *tag_table) []post = {
	let p = parser { buf = buf, pos = 0 };
	expect(&p, '[');
	let posts: []post = alloc([], 8192)!;
	for (true) {
		skip_ws(&p);
		if (peek(&p) == ']') {
			p.pos += 1;
			break;
		};
		append(posts, parse_post(&p, tt))!;
		skip_ws(&p);
		if (peek(&p) == ',') {
			p.pos += 1;
		} else if (peek(&p) == ']') {
			p.pos += 1;
			break;
		} else {
			abort("invalid root array in JSON input");
		};
	};
	return posts;
};

// Turns the per-tag post counts collected while parsing into a compact
// compressed sparse row table, so the scoring loop walks contiguous memory.
fn build_tag_posts(posts: []post, tt: *tag_table) [][]u32 = {
	const ntags = len(tt.tags);
	let starts: []size = alloc([0...], ntags + 1)!;
	let total = 0z;
	for (let t = 0z; t < ntags; t += 1) {
		starts[t] = total;
		total += tt.counts[t]: size;
	};
	starts[ntags] = total;
	let cursors: []size = alloc([0...], ntags)!;
	for (let t = 0z; t < ntags; t += 1) {
		cursors[t] = starts[t];
	};
	let flat: []u32 = alloc([0...], total)!;
	for (let i = 0z; i < len(posts); i += 1) {
		const tags = posts[i].tags;
		for (let k = 0z; k < len(tags); k += 1) {
			const t = tags[k]: size;
			flat[cursors[t]] = i: u32;
			cursors[t] += 1;
		};
	};
	let by_tag: [][]u32 = alloc([], ntags)!;
	for (let t = 0z; t < ntags; t += 1) {
		append(by_tag, flat[starts[t]..starts[t + 1]])!;
	};
	return by_tag;
};

fn top5(posts: []post, by_tag: [][]u32) []u32 = {
	const n = len(posts);
	let related: []u32 = alloc([0...], n * 5)!;
	let counts: []u8 = alloc([0...], n)!;
	let top_score: [5]u8 = [0...];
	let top_idx: [5]u32 = [0...];

	for (let i = 0z; i < n; i += 1) {
		bytes::zero(counts);
		const tags = posts[i].tags;
		for (let tag .. tags) {
			const candidates = by_tag[tag: size];
			for (let c .. candidates) {
				counts[c: size] += 1;
			};
		};
		counts[i] = 0;

		for (let k = 0z; k < 5; k += 1) {
			top_score[k] = 0;
			top_idx[k] = 0;
		};

		for (let j = 0z; j < n; j += 1) {
			const score = counts[j];
			if (score <= top_score[4]) {
				continue;
			};
			let pos = 4z;
			for (pos > 0 && score > top_score[pos - 1]) {
				top_score[pos] = top_score[pos - 1];
				top_idx[pos] = top_idx[pos - 1];
				pos -= 1;
			};
			top_score[pos] = score;
			top_idx[pos] = j: u32;
		};

		for (let k = 0z; k < 5; k += 1) {
			related[i * 5 + k] = top_idx[k];
		};
	};
	return related;
};

const QUOTE: []u8 = ['"'];
const HEX: []u8 = [
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
];

fn write_bytes(out: io::handle, s: []u8) void = {
	io::writeall(out, s)!;
};

fn write_raw(out: io::handle, s: str) void = {
	write_bytes(out, strings::toutf8(s));
};

fn write_json_string(out: io::handle, s: []u8) void = {
	write_bytes(out, QUOTE);
	let start = 0z;
	for (let i = 0z; i < len(s); i += 1) {
		const c = s[i];
		if (c == '"' || c == '\\' || c < 0x20) {
			if (i > start) {
				write_bytes(out, s[start..i]);
			};
			if (c == '"') {
				write_bytes(out, strings::toutf8("\\\""));
			} else if (c == '\\') {
				write_bytes(out, strings::toutf8("\\\\"));
			} else {
				const escaped: [6]u8 = [
					'\\', 'u', '0', '0',
					HEX[c >> 4], HEX[c & 0x0f],
				];
				write_bytes(out, escaped);
			};
			start = i + 1;
		};
	};
	if (start < len(s)) {
		write_bytes(out, s[start..]);
	};
	write_bytes(out, QUOTE);
};

fn write_post(out: io::handle, p: post, tt: *tag_table) void = {
	write_raw(out, "{\"_id\":");
	write_json_string(out, p.id);
	write_raw(out, ",\"title\":");
	write_json_string(out, p.title);
	write_raw(out, ",\"tags\":[");
	for (let i = 0z; i < len(p.tags); i += 1) {
		if (i > 0) {
			write_raw(out, ",");
		};
		write_json_string(out, tt.tags[p.tags[i]: size]);
	};
	write_raw(out, "]}");
};

fn write_output(
	posts: []post,
	tt: *tag_table,
	related: []u32,
) void = {
	const file = os::create("../related_posts_hare.json", 0o644)!;
	let wbuf: [65536]u8 = [0...];
	let out = bufio::init(file, [], wbuf, bufio::flag::MANAGED_HANDLE);

	write_raw(&out, "[\n");
	for (let i = 0z; i < len(posts); i += 1) {
		if (i > 0) {
			write_raw(&out, ",\n");
		};
		write_raw(&out, "{\"_id\":");
		write_json_string(&out, posts[i].id);
		write_raw(&out, ",\"tags\":[");
		for (let k = 0z; k < len(posts[i].tags); k += 1) {
			if (k > 0) {
				write_raw(&out, ",");
			};
			write_json_string(&out, tt.tags[posts[i].tags[k]: size]);
		};
		write_raw(&out, "],\"related\":[");
		for (let k = 0z; k < 5; k += 1) {
			if (k > 0) {
				write_raw(&out, ",");
			};
			write_post(&out, posts[related[i * 5 + k]: size], tt);
		};
		write_raw(&out, "]}");
	};
	write_raw(&out, "\n]\n");
	io::close(&out)!;
};

export fn main() void = {
	const file = os::open("../posts.json")!;
	const stat = os::fstat(file)!;
	let buf: []u8 = alloc([0...], stat.sz)!;
	const nread = match (io::readall(file, buf)) {
	case let n: size =>
		yield n;
	case io::EOF =>
		abort("empty posts.json");
	case io::error =>
		abort("failed to read posts.json");
	};
	io::close(file)!;
	buf = buf[..nread];

	let tt = new_tag_table();
	const posts = parse_posts(buf, &tt);

	const start = time::now(time::clock::MONOTONIC);
	const by_tag = build_tag_posts(posts, &tt);
	const related = top5(posts, by_tag);
	const elapsed = time::diff(start, time::now(time::clock::MONOTONIC));

	fmt::printfln("Processing time (w/o IO): {:.3}ms",
		(elapsed: f64) / 1000000.0)!;

	write_output(posts, &tt, related);
};
