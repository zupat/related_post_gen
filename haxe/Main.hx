// Haxe implementation, compiled to native C++ with hxcpp.
//
// The algorithm mirrors the other single-threaded implementations: build a
// tag -> post-index map once, then for every post count shared tags through the
// map and keep the five highest counts with a small insertion sort.

typedef Post = {
    var _id:String;
    var title:String;
    var tags:Array<String>;
}

typedef RelatedPosts = {
    var _id:String;
    var tags:Array<String>;
    var related:Array<Post>;
}

class Main {
    static inline var TOP_N:Int = 5;

    static function main() {
        var posts:Array<Post> = cast haxe.Json.parse(sys.io.File.getContent("../posts.json"));
        var postCount = posts.length;

        var start = haxe.Timer.stamp();

        // Tag -> indices of the posts carrying that tag.
        var tagPosts = new Map<String, Array<Int>>();
        for (i in 0...postCount) {
            for (tag in posts[i].tags) {
                var indices = tagPosts[tag];
                if (indices == null) {
                    indices = [];
                    tagPosts[tag] = indices;
                }
                indices.push(i);
            }
        }

        var taggedCount = [for (_ in 0...postCount) 0];
        var results = new Array<RelatedPosts>();

        for (i in 0...postCount) {
            for (tag in posts[i].tags) {
                for (other in tagPosts[tag]) {
                    taggedCount[other]++;
                }
            }
            taggedCount[i] = 0; // never recommend the post itself

            var topCount = [0, 0, 0, 0, 0];
            var topIndex = [0, 0, 0, 0, 0];
            var minTags = 0;

            for (j in 0...postCount) {
                var count = taggedCount[j];
                if (count > minTags) {
                    var pos = TOP_N - 2;
                    while (pos >= 0 && count > topCount[pos]) {
                        topCount[pos + 1] = topCount[pos];
                        topIndex[pos + 1] = topIndex[pos];
                        pos--;
                    }
                    topCount[pos + 1] = count;
                    topIndex[pos + 1] = j;
                    minTags = topCount[TOP_N - 1];
                }
            }

            var related = new Array<Post>();
            for (k in 0...TOP_N) {
                related.push(posts[topIndex[k]]);
            }

            results.push({
                _id: posts[i]._id,
                tags: posts[i].tags,
                related: related
            });

            for (j in 0...postCount) {
                taggedCount[j] = 0;
            }
        }

        var elapsedMs = Math.round((haxe.Timer.stamp() - start) * 1000);
        Sys.println('Processing time (w/o IO): ${elapsedMs}ms');

        writeResults(results);
    }

    // haxe.Json.stringify emits surrogate pairs one code unit at a time on the
    // C++ target, turning non-BMP characters (for example emoji) into U+FFFD.
    // Write the JSON directly and copy strings in chunks instead: the hxcpp
    // UCS-2 -> UTF-8 conversion then keeps surrogate pairs intact.
    static function writeResults(results:Array<RelatedPosts>) {
        var json = new StringBuf();
        json.add('[');

        for (i in 0...results.length) {
            if (i > 0) {
                json.add(',');
            }

            var post = results[i];
            json.add('{"_id":');
            addJsonString(json, post._id);
            json.add(',"tags":');
            addJsonStringArray(json, post.tags);
            json.add(',"related":[');

            for (k in 0...TOP_N) {
                if (k > 0) {
                    json.add(',');
                }
                var related = post.related[k];
                json.add('{"_id":');
                addJsonString(json, related._id);
                json.add(',"title":');
                addJsonString(json, related.title);
                json.add(',"tags":');
                addJsonStringArray(json, related.tags);
                json.add('}');
            }

            json.add(']}');
        }

        json.add(']');
        sys.io.File.saveContent("../related_posts_haxe.json", json.toString());
    }

    static function addJsonStringArray(buf:StringBuf, values:Array<String>) {
        buf.add('[');
        for (i in 0...values.length) {
            if (i > 0) {
                buf.add(',');
            }
            addJsonString(buf, values[i]);
        }
        buf.add(']');
    }

    static function addJsonString(buf:StringBuf, s:String) {
        buf.add('"');

        var start = 0;
        var length = s.length;
        for (i in 0...length) {
            var c = StringTools.unsafeCodeAt(s, i);
            var escape:String = null;

            switch (c) {
                case '"'.code:
                    escape = '\\"';
                case '\\'.code:
                    escape = '\\\\';
                case 8:
                    escape = '\\b';
                case 9:
                    escape = '\\t';
                case 10:
                    escape = '\\n';
                case 12:
                    escape = '\\f';
                case 13:
                    escape = '\\r';
            }

            if (escape != null) {
                if (i > start) {
                    buf.add(s.substring(start, i));
                }
                buf.add(escape);
                start = i + 1;
            } else if (c < 0x20) {
                if (i > start) {
                    buf.add(s.substring(start, i));
                }
                var hex = '0123456789abcdef';
                buf.add('\\u00');
                buf.add(hex.charAt(c >> 4));
                buf.add(hex.charAt(c & 15));
                start = i + 1;
            }
        }

        if (start < length) {
            buf.add(s.substring(start));
        }
        buf.add('"');
    }
}
