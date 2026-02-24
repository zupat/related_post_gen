use std::{
    cell::RefCell,
    collections::VecDeque,
    fs::OpenOptions,
    hint,
    io::{self, BufWriter},
    time::Instant,
};

use rayon::prelude::*;

mod types;
mod utils;

use types::{Post, RelatedPosts, NUM_TOP_ITEMS};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

const INPUT_FILE: &str = "../posts.json";
const OUTPUT_FILE: &str = "../related_posts_rust_con.json";
const CHUNK_SIZE: usize = 64;

fn main() -> io::Result<()> {
    let num_cpus = num_cpus::get_physical();
    rayon::ThreadPoolBuilder::new()
        .num_threads(num_cpus)
        .build_global()
        .unwrap();

    let json_str = std::fs::read_to_string(INPUT_FILE)?;
    let mut posts: VecDeque<Post> = serde_json::from_str(&json_str).unwrap();

    let start = hint::black_box(Instant::now());
    let related_posts = get_related(posts.make_contiguous());
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

fn get_related<'a>(posts: &'a [Post]) -> Vec<RelatedPosts<'a>> {
    thread_local! {
        static POST_COUNT: RefCell<Vec<u8>> = panic!("!");
    }

    let padded_len = posts.len().next_multiple_of(CHUNK_SIZE);
    rayon::broadcast(|_| {
        POST_COUNT.set(vec![0u8; padded_len]);
    });

    let post_tags_map = utils::get_post_tags_map(posts);

    posts
        .par_iter()
        .enumerate()
        .map(|(idx, post)| {
            POST_COUNT.with_borrow_mut(|tagged_post_count| {
                for tag in &post.tags {
                    if let Some(tag_posts) = post_tags_map.get(tag) {
                        for &other_post_idx in tag_posts {
                            tagged_post_count[other_post_idx as usize] += 1;
                        }
                    }
                }
                tagged_post_count[idx] = 0;

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
                    chunk.fill(0);
                }

                RelatedPosts {
                    id: post.id,
                    tags: &post.tags,
                    related: topk.map(|(_, index)| &posts[index as usize]),
                }
            })
        })
        .collect()
}
