import json
import os
import time

const top_posts_count = 5

struct Post {
	id    string   @[json: '_id']
	title string   @[json: 'title']
	tags  []string @[json: 'tags']
}

struct RelatedPosts {
	id      string                @[json: '_id']
	tags    []string
	related [top_posts_count]Post
}

struct Ctx {
	posts   []Post
	tag_map map[string][]int
}

fn compute(ctx &Ctx) []RelatedPosts {
	mut tagged_post_count := []int{len: ctx.posts.len}
	mut out := []RelatedPosts{cap: ctx.posts.len}

	for i, post in ctx.posts {
		for j in 0 .. tagged_post_count.len {
			tagged_post_count[j] = 0
		}

		for tag in post.tags {
			for post_index in ctx.tag_map[tag] {
				tagged_post_count[post_index] += 1
			}
		}

		tagged_post_count[i] = 0

		mut top5 := [top_posts_count * 2]int{}
		mut min_tags := 0

		for j, count in tagged_post_count {
			if count > min_tags {
				mut upper_bound := (top_posts_count - 2) * 2

				mut count_int := int(count)
				for upper_bound >= 0 && count_int > top5[upper_bound] {
					top5[upper_bound + 2] = top5[upper_bound]
					top5[upper_bound + 3] = top5[upper_bound + 1]
					upper_bound -= 2
				}

				insert_pos := upper_bound + 2
				top5[insert_pos] = count_int
				top5[insert_pos + 1] = j

				min_tags = top5[top_posts_count * 2 - 2]
			}
		}

		mut top_posts := [top_posts_count]Post{}

		for j := 0; j < top_posts_count; j += 1 {
			index := top5[j * 2 + 1]
			top_posts[j] = ctx.posts[index]
		}

		out << RelatedPosts{
			id: post.id
			tags: post.tags
			related: top_posts
		}
	}

	return out
}

fn main() {
	json_str := os.read_file('../posts.json') or {
		eprintln('Failed to read file ${err}')
		return
	}
	posts := json.decode([]Post, json_str) or {
		eprintln('Failed to parse json ${err}')
		return
	}

	mut tag_map := map[string][]int{}
	for i, post in posts {
		for tag in post.tags {
			tag_map[tag] << i
		}
	}

	start := time.now()

	ctx := Ctx{
		posts: posts
		tag_map: tag_map
	}
	mut all_related_posts := compute(&ctx)

	end := time.now()

	println('Processing time (w/o IO): ${end - start}')

	json_str_out := json.encode(all_related_posts)

	os.write_file('../related_posts_v.json', json_str_out) or {
		eprintln('Failed to write file ${err}')
		return
	}
}
