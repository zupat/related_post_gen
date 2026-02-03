use rustc_hash::{FxBuildHasher, FxHashMap};
use serde::{Deserialize, Serialize};
use std::{
    fs::OpenOptions,
    hint,
    io::{self, BufWriter},
    time::Instant,
};

const NUM_TOP_ITEMS: usize = 5;
const INPUT_FILE: &str = "../posts.json";
const OUTPUT_FILE: &str = "../related_posts_rust.json";

#[derive(Serialize, Deserialize)]
#[repr(align(64))]
struct Post<'a> {
    #[serde(rename = "_id")]
    id: &'a str,
    title: &'a str,
    tags: Vec<&'a str>,
}

#[derive(Serialize)]
#[repr(align(64))]
struct RelatedPosts<'a> {
    #[serde(rename = "_id")]
    id: &'a str,
    tags: &'a [&'a str],
    related: [&'a Post<'a>; NUM_TOP_ITEMS],
}

fn main() -> io::Result<()> {
    let json_str = std::fs::read_to_string(INPUT_FILE)?;
    let posts: Vec<Post> = serde_json::from_str(&json_str).unwrap();

    let start = Instant::now();

    let mut post_tags_map =
        FxHashMap::<&str, Vec<u32>>::with_capacity_and_hasher(128, FxBuildHasher);
    for (post_idx, post) in posts.iter().enumerate() {
        for tag in &post.tags {
            post_tags_map
                .entry(tag)
                .or_insert_with(|| Vec::with_capacity(1024))
                .push(post_idx as u32);
        }
    }

    const CHUNK_SIZE: usize = 64;
    let mut tagged_post_count = vec![0u8; posts.len().next_multiple_of(CHUNK_SIZE)];
    let related_posts: Vec<_> = posts
        .iter()
        .enumerate()
        .map(|(post_idx, post)| {
            for tag in &post.tags {
                for other_post_idx in &post_tags_map[tag] {
                    tagged_post_count[*other_post_idx as usize] += 1;
                }
            }
            tagged_post_count[post_idx] = 0; // don't recommend the same post

            let mut topk = [(0u8, 0u32); NUM_TOP_ITEMS];
            let mut min_tags = 0u8;
            for (c, chunk) in tagged_post_count.chunks_mut(CHUNK_SIZE).enumerate() {
                let mut process_chunk = false;
                for &count in chunk.iter() {
                    process_chunk |= count > min_tags;
                }
                if process_chunk {
                    for (j, &count) in chunk.iter().enumerate() {
                        if count > min_tags {
                            // Insert at end, bubble up
                            topk[NUM_TOP_ITEMS - 1] = (count, (c * CHUNK_SIZE + j) as u32);
                            for i in (0..NUM_TOP_ITEMS - 1).rev() {
                                if topk[i].0 >= count {
                                    break;
                                }
                                topk.swap(i, i + 1);
                            }
                            min_tags = topk[NUM_TOP_ITEMS - 1].0;
                        }
                    }
                }
                chunk.fill(0); // zero while still in L1 cache
            }

            RelatedPosts {
                id: post.id,
                tags: &post.tags,
                related: topk.map(|(_, index)| &posts[index as usize]),
            }
        })
        .collect();

    // Tell compiler to not delay now() until print is eval'ed.
    let end = hint::black_box(Instant::now());

    println!("Processing time (w/o IO): {:?}", end.duration_since(start));

    let output_file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(OUTPUT_FILE)?;
    let writer = BufWriter::new(output_file);
    serde_json::to_writer(writer, &related_posts).unwrap();

    Ok(())
}
