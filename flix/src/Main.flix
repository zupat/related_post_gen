use Util.Json
use Util.Json.Json
use Util.Json.Json.{JArray, JObject, JString}

///
/// Returns the field `k` of the JSON object `j`.
///
def getField(k: String, j: Json): Json = match j {
    case JObject(m) => match Map.get(k, m) {
        case Some(v) => v
        case None    => bug!("missing field '${k}'")
    }
    case _ => bug!("expected a JSON object")
}

def asString(j: Json): String = match j {
    case JString(s) => s
    case _          => bug!("expected a JSON string")
}

def asStrings(j: Json): Vector[String] = match j {
    case JArray(v) => Vector.map(asString, v)
    case _         => bug!("expected a JSON array")
}

def parsePosts(s: String): Vector[Json] = match Json.parse(s) {
    case Ok(JArray(v)) => v
    case Ok(_)         => bug!("expected the top-level JSON value to be an array")
    case Err(e)        => bug!("JSON parse error: ${e}")
}

///
/// Appends `s` to `sb` as a JSON string literal.
///
def writeString(s: String, sb: StringBuilder[r]): Unit \ r =
    StringBuilder.appendString(Json.toCompactString(Json.JString(s)), sb)

def writeStrings(xs: Vector[String], sb: StringBuilder[r]): Unit \ r = {
    StringBuilder.append('[', sb);
    Vector.forEachWithIndex((i, x) -> {
        if (i > 0) { StringBuilder.append(',', sb) };
        writeString(x, sb)
    }, xs);
    StringBuilder.append(']', sb)
}

def writePost(i: Int32, ids: Vector[String], titles: Vector[String], tags: Vector[Vector[String]], sb: StringBuilder[r]): Unit \ r = {
    StringBuilder.appendString("{\"_id\":", sb);
    writeString(Vector.get(i, ids), sb);
    StringBuilder.appendString(",\"title\":", sb);
    writeString(Vector.get(i, titles), sb);
    StringBuilder.appendString(",\"tags\":", sb);
    writeStrings(Vector.get(i, tags), sb);
    StringBuilder.append('}', sb)
}

///
/// Computes the top-5 related posts for every post and writes the result file.
///
def runBench(ids: Vector[String], titles: Vector[String], tags: Vector[Vector[String]]): Unit \ IO =
    Time.Clock.runWithIO(() -> {
        let n = Vector.length(ids);

        let top: Vector[Int32] = region rc {
            let tagMap = MutMap.empty(rc);

            foreach (i <- Vector.range(0, n)) {
                foreach (tag <- Vector.get(i, tags)) {
                    match MutMap.get(tag, tagMap) {
                        case None =>
                            let postsWithTag = MutList.empty(rc);
                            MutList.push(i, postsWithTag);
                            MutMap.put(tag, postsWithTag, tagMap)
                        case Some(postsWithTag) => MutList.push(i, postsWithTag)
                    }
                }
            };

            let counts = Array.repeat(rc, n, 0);
            let touched = Array.repeat(rc, n, 0);
            let topIndices = Array.repeat(rc, n * 5, 0);

            let start = Time.Clock.now();

            foreach (i <- Vector.range(0, n)) {
                let touchedCount = Ref.fresh(rc, 0);

                foreach (tag <- Vector.get(i, tags)) {
                    match MutMap.get(tag, tagMap) {
                        case None => ()
                        case Some(postsWithTag) =>
                            foreach (j <- postsWithTag) {
                                let c = Array.get(j, counts);
                                if (c == 0) {
                                    Array.put(j, Ref.get(touchedCount), touched);
                                    Ref.put(Ref.get(touchedCount) + 1, touchedCount)
                                };
                                Array.put(c + 1, j, counts)
                            }
                    }
                };

                // Do not count the post itself.
                Array.put(0, i, counts);

                let topCounts = Array.repeat(rc, 5, 0);
                let topPosts = Array.repeat(rc, 5, 0);

                def tryInsert(pos: Int32, c: Int32, j: Int32) =
                    if (pos < 0 or c <= Array.get(pos, topCounts))
                        ()
                    else {
                        if (pos < 4) {
                            Array.put(Array.get(pos, topCounts), pos + 1, topCounts);
                            Array.put(Array.get(pos, topPosts), pos + 1, topPosts)
                        };
                        Array.put(c, pos, topCounts);
                        Array.put(j, pos, topPosts);
                        tryInsert(pos - 1, c, j)
                    };

                def scanTop(t: Int32) =
                    if (t >= Ref.get(touchedCount))
                        ()
                    else {
                        let j = Array.get(t, touched);
                        if (j != i) {
                            let c = Array.get(j, counts);
                            if (c > 0) {
                                tryInsert(4, c, j)
                            }
                        };
                        // Clear the counter for the next post.
                        Array.put(0, j, counts);
                        scanTop(t + 1)
                    };

                scanTop(0);

                foreach (r <- Vector.range(0, 5)) {
                    Array.put(Array.get(r, topPosts), i * 5 + r, topIndices)
                }
            };

            let end = Time.Clock.now();
            println("Processing time (w/o IO): ${end - start}ms");

            Array.toVector(topIndices)
        };

        let sb = region rc {
            let sb = StringBuilder.empty(rc);
            StringBuilder.append('[', sb);
            foreach (i <- Vector.range(0, n)) {
                if (i > 0) { StringBuilder.append(',', sb) };
                StringBuilder.appendString("{\"_id\":", sb);
                writeString(Vector.get(i, ids), sb);
                StringBuilder.appendString(",\"tags\":", sb);
                writeStrings(Vector.get(i, tags), sb);
                StringBuilder.appendString(",\"related\":[", sb);
                foreach (r <- Vector.range(0, 5)) {
                    if (r > 0) { StringBuilder.append(',', sb) };
                    writePost(Vector.get(i * 5 + r, top), ids, titles, tags, sb)
                };
                StringBuilder.appendString("]}", sb)
            };
            StringBuilder.append(']', sb);
            StringBuilder.toString(sb)
        };

        Fs.FileWrite.runWithIO(() -> {
            match Fs.FileWrite.write(str = sb, "../related_posts_flix.json") {
                case Ok(_)  => ()
                case Err(e) => bug!("failed to write output: ${e}")
            }
        })
    })

def main(): Unit \ IO =
    Fs.FileRead.runWithIO(() -> {
        match Fs.FileRead.read("../posts.json") {
            case Err(e) => bug!("failed to read posts: ${e}")
            case Ok(s) =>
                let posts = parsePosts(s);
                let ids = Vector.map(p -> asString(getField("_id", p)), posts);
                let titles = Vector.map(p -> asString(getField("title", p)), posts);
                let tags = Vector.map(p -> asStrings(getField("tags", p)), posts);
                runBench(ids, titles, tags)
        }
    })
