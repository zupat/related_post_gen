import gleam/dynamic
import gleam/json

pub type Post {
  Post(id: String, title: String, tags: List(String))
}

pub type RelatedPost {
  RelatedPost(id: String, title: String, tags: List(String), related: List(Post))
}

pub fn decode(data: dynamic.Dynamic) -> Result(Post, List(dynamic.DecodeError)) {
  dynamic.decode3(
    Post,
    dynamic.field("_id", of: dynamic.string),
    dynamic.field("title", of: dynamic.string),
    dynamic.field("tags", of: dynamic.list(of: dynamic.string)),
  )(data)
}

pub fn post_to_json(post: Post) -> json.Json {
  json.object([
    #("_id", json.string(post.id)),
    #("title", json.string(post.title)),
    #("tags", json.array(post.tags, of: json.string)),
  ])
}

pub fn related_to_json(post: RelatedPost) -> json.Json {
  json.object([
    #("_id", json.string(post.id)),
    #("title", json.string(post.title)),
    #("tags", json.array(post.tags, of: json.string)),
    #("related", json.array(post.related, of: post_to_json)),
  ])
}
