import gleam/dynamic
import gleam/io
import gleam/int
import gleam/list
import gleam/bool
import gleam/set
import gleam/pair
import gleam/float
import simplifile.{read, write}
import birl/time
import gleam/json
import post.{type Post, type RelatedPost, RelatedPost}

fn to_related_post(post: Post, posts: List(Post)) -> RelatedPost {
  RelatedPost(
    post.id,
    set.to_list(post.tags),
    similar_posts(to: post, among: posts),
  )
}

fn similar_posts(to post: Post, among posts: List(Post)) -> List(Post) {
  let other_posts_with_similarity = {
    use other_post <- list.filter_map(posts)
    use <- bool.guard(when: other_post.id == post.id, return: Error(Nil))
    let similarity = count_shared_tags(post, other_post)
    Ok(#(similarity, other_post))
  }
  take_top_n(other_posts_with_similarity)
}

fn count_shared_tags(one: Post, other: Post) -> Int {
  set.size(set.intersection(one.tags, other.tags))
}

const top_n = 5

fn take_top_n(list: List(#(Int, a))) -> List(a) {
  list.sort(list, fn(one, other) { int.compare(other.0, one.0) })
  |> list.take(top_n)
  |> list.map(pair.second)
}

pub fn main() {
  let assert Ok(raw_posts) = read("../posts.json")
  let assert Ok(posts) =
    json.decode(raw_posts, using: dynamic.list(post.decode))

  let start = time.monotonic_now()
  let related_posts =
    posts
    |> list.take(1)
    |> list.map(to_related_post(_, posts))
  let end = time.monotonic_now()

  let took = float.to_string(int.to_float(end - start) /. 1000.0)
  io.println("Processing time (w/o IO): " <> { took } <> "ms")

  let all_related_json =
    json.array(related_posts, of: post.related_to_json)
    |> json.to_string

  let assert Ok(_) = write(all_related_json, "../related_posts_gleam.json")
}
