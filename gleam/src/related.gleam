import birl/time
import gleam/dynamic
import gleam/float
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/map
import gleam/result
import post.{type Post, type RelatedPost, RelatedPost}
import simplifile

// Erlang's mutable counters. Gleam has no mutable arrays, and this is the
// same primitive the Erlang implementation uses for its hot loop.
@external(erlang, "erts_internal", "counters_new")
fn counters_new(size: Int) -> dynamic.Dynamic

@external(erlang, "erts_internal", "counters_add")
fn counters_add(counters: dynamic.Dynamic, index: Int, increment: Int) -> Nil

@external(erlang, "erts_internal", "counters_put")
fn counters_put(counters: dynamic.Dynamic, index: Int, value: Int) -> Nil

@external(erlang, "erts_internal", "counters_get")
fn counters_get(counters: dynamic.Dynamic, index: Int) -> Int

fn build_tag_map(posts: List(Post)) -> map.Map(String, List(Int)) {
  let #(_, tag_map) =
    list.fold(posts, #(0, map.new()), fn(acc, post) {
      let #(index, tag_map) = acc
      let tag_map =
        list.fold(post.tags, tag_map, fn(tag_map, tag) {
          let ids = case map.get(tag_map, tag) {
            Ok(ids) -> [index, ..ids]
            Error(_) -> [index]
          }
          map.insert(tag_map, tag, ids)
        })
      #(index + 1, tag_map)
    })
  tag_map
}

fn insert_top(top: List(#(Int, Int)), item: #(Int, Int)) -> List(#(Int, Int)) {
  case top {
    [] -> [item]
    [first, ..rest] ->
      case item.0 > first.0 {
        True -> [item, ..top]
        False -> [first, ..insert_top(rest, item)]
      }
  }
}

fn scan(
  index: Int,
  size: Int,
  counters: dynamic.Dynamic,
  top: List(#(Int, Int)),
  minimum: Int,
) -> List(#(Int, Int)) {
  case index > size {
    True -> top
    False -> {
      let count = counters_get(counters, index)
      case count > minimum {
        True -> {
          let top = insert_top(top, #(count, index)) |> list.take(5)
          let minimum = case list.last(top) {
            Ok(#(count, _)) -> count
            Error(_) -> 0
          }
          scan(index + 1, size, counters, top, minimum)
        }
        False -> scan(index + 1, size, counters, top, minimum)
      }
    }
  }
}

fn related_indices(
  self: Int,
  tags: List(String),
  tag_map: map.Map(String, List(Int)),
  size: Int,
) -> List(Int) {
  let counters = counters_new(size)
  list.each(tags, fn(tag) {
    case map.get(tag_map, tag) {
      Ok(indices) -> list.each(indices, fn(index) {
        counters_add(counters, index + 1, 1)
      })
      Error(_) -> Nil
    }
  })
  counters_put(counters, self + 1, 0)
  scan(1, size, counters, [], 0)
  |> list.map(fn(pair) { pair.1 - 1 })
}

fn compute(posts: List(Post)) -> List(RelatedPost) {
  let size = list.length(posts)
  let tag_map = build_tag_map(posts)
  let by_index =
    list.index_map(posts, fn(index, post) { #(index, post) })
    |> map.from_list

  let #(_, related) =
    list.index_fold(posts, #(0, []), fn(acc, post, index) {
      let #(_, related) = acc
      let indices = related_indices(index, post.tags, tag_map, size)
      let posts =
        list.map(indices, fn(index) {
          case map.get(by_index, index) {
            Ok(post) -> post
            Error(_) -> post
          }
        })
      #(
        index + 1,
        [RelatedPost(post.id, post.title, post.tags, posts), ..related],
      )
    })
  list.reverse(related)
}

pub fn main() {
  let assert Ok(raw_posts) = simplifile.read(from: "../posts.json")
  let assert Ok(posts) =
    json.decode(from: raw_posts, using: dynamic.list(of: post.decode))

  let start = time.monotonic_now()
  let related = compute(posts)
  let end = time.monotonic_now()

  let took = float.to_string(int.to_float(end - start) /. 1000.0)
  io.println("Processing time (w/o IO): " <> took <> "ms")

  let out =
    json.array(related, of: post.related_to_json)
    |> json.to_string

  simplifile.write(out, to: "../related_posts_gleam.json")
  |> result.unwrap(Nil)
}
