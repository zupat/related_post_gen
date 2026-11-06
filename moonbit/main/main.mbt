// Related posts benchmark for MoonBit.
//
// Parses ../posts.json, maps each tag to the posts carrying it, and writes the
// five posts with the most shared tags for every post to
// ../related_posts_moonbit.json.

///|
struct Post {
  id : String
  title : String
  tags : Array[String]
}

///|
fn parse_posts(content : String) -> Array[Post] {
  let posts : Array[Post] = []
  let parsed = @json.parse(content) catch { _ => return posts }
  match parsed {
    Json::Array(items) =>
      for item in items {
        match item {
          Json::Object(obj) => {
            let id = match obj.get("_id") {
              Some(Json::String(s)) => s
              _ => ""
            }
            let title = match obj.get("title") {
              Some(Json::String(s)) => s
              _ => ""
            }
            let tags : Array[String] = match obj.get("tags") {
              Some(Json::Array(ts)) => {
                let out : Array[String] = []
                for t in ts {
                  match t {
                    Json::String(s) => out.push(s)
                    _ => ()
                  }
                }
                out
              }
              _ => []
            }
            posts.push(Post::{ id, title, tags, })
          }
          _ => ()
        }
      }
    _ => ()
  }
  posts
}

///|
fn post_to_json(p : Post) -> Json {
  let tags = p.tags.map(fn(t) { Json::string(t) })
  Json::object({
    "_id": Json::string(p.id),
    "title": Json::string(p.title),
    "tags": Json::array(tags),
  })
}

///|
fn main {
  let content = @fs.read_file_to_string("../posts.json") catch { _ => "" }
  if content.is_empty() {
    println("failed to read ../posts.json")
    return
  }

  let posts = parse_posts(content)
  let n = posts.length()

  let start = @bench.monotonic_clock_start()

  let tag_map : Map[String, Array[Int32]] = Map([])
  for i = 0; i < n; i = i + 1 {
    for tag in posts[i].tags {
      match tag_map.get(tag) {
        Some(list) => list.push(i)
        None => tag_map.set(tag, [i])
      }
    }
  }

  let counts = Array::make(n, 0)
  let top_indices = Array::make(n * 5, 0)
  let top5 = Array::make(10, 0)

  for i = 0; i < n; i = i + 1 {
    for tag in posts[i].tags {
      match tag_map.get(tag) {
        Some(list) =>
          for other in list {
            counts[other] = counts[other] + 1
          }
        None => ()
      }
    }

    counts[i] = 0 // Don't count self
    for k = 0; k < 10; k = k + 1 {
      top5[k] = 0
    }
    let mut min_tags = 0
    // Scan every post and clear the counters in the same pass: the reset the
    // reference implementation does separately is folded in here.
    for j = 0; j < n; j = j + 1 {
      let c = counts[j]
      counts[j] = 0
      if c > min_tags {
        let mut ub = 6
        while ub >= 0 && c > top5[ub] {
          top5[ub + 2] = top5[ub]
          top5[ub + 3] = top5[ub + 1]
          ub = ub - 2
        }
        let ip = ub + 2
        top5[ip] = c
        top5[ip + 1] = j
        min_tags = top5[8]
      }
    }

    for k = 0; k < 5; k = k + 1 {
      top_indices[i * 5 + k] = top5[k * 2 + 1]
    }
  }

  let elapsed_us = @bench.monotonic_clock_end(start)
  println("Processing time (w/o IO): \{elapsed_us / 1000.0}ms")

  let post_jsons = posts.map(post_to_json)
  let out : Array[Json] = Array::make(n, Json::null())
  for i = 0; i < n; i = i + 1 {
    let related : Array[Json] = Array::make(5, Json::null())
    for k = 0; k < 5; k = k + 1 {
      related[k] = post_jsons[top_indices[i * 5 + k]]
    }
    out[i] = Json::object({
      "_id": Json::string(posts[i].id),
      "tags": Json::array(posts[i].tags.map(fn(t) { Json::string(t) })),
      "related": Json::array(related),
    })
  }
  let text = Json::array(out).stringify()
  @fs.write_string_to_file("../related_posts_moonbit.json", text) catch {
    _ => println("failed to write ../related_posts_moonbit.json")
  }
}
