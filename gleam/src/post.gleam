import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic}
import gleam/json.{type Json}
import gleam/set.{type Set}
import gleam/result

pub type Post {
  Post(id: PostId, title: String, tags: Set(Tag))
}

pub opaque type Tag {
  Tag(string: String)
}

pub opaque type PostId {
  PostId(string: String)
}

pub type RelatedPost {
  RelatedPost(id: PostId, tags: List(Tag), related: List(Post))
}

// JSON ENCODERS ---------------------------------------------------------------

pub fn related_to_json(related_post: RelatedPost) -> Json {
  json.object([
    #("_id", id_to_json(related_post.id)),
    #("tags", json.array(related_post.tags, of: tag_to_json)),
    #("related", json.array(related_post.related, of: post_to_json)),
  ])
}

fn post_to_json(post: Post) -> Json {
  let Post(id, title, tags) = post
  json.object([
    #("_id", id_to_json(id)),
    #("title", json.string(title)),
    #("tags", json.array(set.to_list(tags), of: tag_to_json)),
  ])
}

fn tag_to_json(tag: Tag) -> Json {
  let Tag(tag) = tag
  json.string(tag)
}

fn id_to_json(id: PostId) -> Json {
  let PostId(id) = id
  json.string(id)
}

// DECODERS --------------------------------------------------------------------

pub fn decode(data: Dynamic) -> Result(Post, List(DecodeError)) {
  let post_id_decoder = fn(data) { result.map(dynamic.string(data), PostId) }
  let tag_decoder = fn(data) { result.map(dynamic.string(data), Tag) }
  dynamic.decode3(
    Post,
    dynamic.field("_id", of: post_id_decoder),
    dynamic.field("title", of: dynamic.string),
    dynamic.field("tags", of: decode_set_from_list(tag_decoder)),
  )(data)
}

fn decode_set_from_list(with decoder: Decoder(a)) -> Decoder(Set(a)) {
  fn(data) {
    dynamic.list(of: decoder)(data)
    |> result.map(set.from_list)
  }
}
