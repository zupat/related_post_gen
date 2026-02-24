use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::{
    fs::OpenOptions,
    hint,
    io::{self, BufWriter},
    time::Instant,
};

const NUM_TOP_ITEMS: usize = 5;
const INPUT_FILE: &str = "../posts.json";
const OUTPUT_FILE: &str = "../related_posts_rust_ho.json";

#[derive(Serialize, Deserialize)]
struct Post<'a> {
    #[serde(rename = "_id")]
    id: &'a str,
    title: &'a str,
    tags: Vec<&'a str>,
}

#[derive(Serialize)]
struct RelatedPosts<'a> {
    #[serde(rename = "_id")]
    id: &'a str,
    tags: &'a [&'a str],
    related: [&'a Post<'a>; NUM_TOP_ITEMS],
}

type PostIdx = u32;

struct TopNTracker {
    resolved: u8, // top_ns[0..resolved] are locked from higher cardinalities
    total: u8,    // top_ns[resolved..total] are sorted ascending
}

impl TopNTracker {
    fn new() -> Self {
        Self {
            resolved: 0,
            total: 0,
        }
    }
}

fn main() -> io::Result<()> {
    let json_str = std::fs::read_to_string(INPUT_FILE)?;
    let posts: Vec<Post> = serde_json::from_str(&json_str).unwrap();

    let start = Instant::now();

    let related_posts = gen_related_posts(&posts);

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

fn gen_related_posts<'a>(posts: &'a [Post]) -> Vec<RelatedPosts<'a>> {
    if posts.is_empty() {
        return Vec::new();
    }

    // Phase A: Map tags to integer IDs
    let mut tag_id_map: HashMap<&str, u8> = HashMap::with_capacity(128);
    let mut next_tid: u8 = 0;
    let mut k_max: usize = 0;

    for post in posts {
        k_max = k_max.max(post.tags.len());
        for &tag in &post.tags {
            tag_id_map.entry(tag).or_insert_with(|| {
                let id = next_tid;
                next_tid = next_tid.checked_add(1).expect("more than 256 unique tags");
                id
            });
        }
    }

    // Build sorted tag ID vectors per post
    let post_tids: Vec<Vec<u8>> = posts
        .iter()
        .map(|post| {
            let mut tids: Vec<u8> = post.tags.iter().map(|t| tag_id_map[t]).collect();
            tids.sort_unstable();
            tids
        })
        .collect();

    // Fallback to standard algorithm if any post has >8 tags (powerset keys must fit in u64)
    if k_max > 8 {
        return fallback_standard(posts);
    }

    // Phase B: Build powerset combination table
    let combo_table = build_combo_table(k_max);

    // Phase C: Initialize trackers
    let mut trackers: Vec<TopNTracker> = (0..posts.len()).map(|_| TopNTracker::new()).collect();
    let mut top_ns: Vec<[PostIdx; NUM_TOP_ITEMS]> = vec![[0; NUM_TOP_ITEMS]; posts.len()];

    // Phase D: Main loop — process cardinalities from k_max down to 1
    let mut more_to_do = true;
    for k in (1..=k_max).rev() {
        if !more_to_do {
            break;
        }

        // Build combination → post list map, collect groups with >=2 posts
        let intersection_lists = find_intersections(k, &post_tids, &combo_table);

        // Disseminate intersections to top-N trackers
        disseminate(&intersection_lists, &mut trackers, &mut top_ns);

        // Promote: resolved = total, check if more work needed
        more_to_do = false;
        for tracker in trackers.iter_mut() {
            tracker.resolved = tracker.total;
            if (tracker.resolved as usize) < NUM_TOP_ITEMS {
                more_to_do = true;
            }
        }
    }

    // Phase E: Build output
    posts
        .iter()
        .enumerate()
        .map(|(i, post)| {
            let total = trackers[i].total as usize;
            let mut related = [&posts[0]; NUM_TOP_ITEMS];
            for j in 0..total.min(NUM_TOP_ITEMS) {
                related[j] = &posts[top_ns[i][j] as usize];
            }
            RelatedPosts {
                id: post.id,
                tags: &post.tags,
                related,
            }
        })
        .collect()
}

/// Build combination table: table[n][k] = list of k-element index subsets of 0..n
fn build_combo_table(k_max: usize) -> Vec<Vec<Vec<Vec<u8>>>> {
    let mut table: Vec<Vec<Vec<Vec<u8>>>> = vec![vec![Vec::new(); k_max + 1]; k_max + 1];

    // Empty subset for all set sizes
    for n in 0..=k_max {
        table[n][0] = vec![vec![]];
    }

    for bitset in 1u32..(1u32 << k_max) {
        let k = bitset.count_ones() as usize;
        if k > k_max {
            continue;
        }

        // Extract set bit positions
        let combo: Vec<u8> = (0..k_max as u8)
            .filter(|&i| bitset & (1 << i) != 0)
            .collect();

        // The minimum set size this combo applies to is highest_bit + 1
        let min_n = (32 - bitset.leading_zeros()) as usize;

        for n in min_n..=k_max {
            table[n][k].push(combo.clone());
        }
    }

    table
}

fn find_intersections(
    k: usize,
    post_tids: &[Vec<u8>],
    combo_table: &[Vec<Vec<Vec<u8>>>],
) -> Vec<Vec<PostIdx>> {
    if k <= 4 {
        find_intersections_packed::<u32>(k, post_tids, combo_table)
    } else {
        find_intersections_packed::<u64>(k, post_tids, combo_table)
    }
}

trait PackKey: Eq + std::hash::Hash + Copy {
    fn pack(tids: &[u8], combo: &[u8]) -> Self;
}

impl PackKey for u32 {
    #[inline]
    fn pack(tids: &[u8], combo: &[u8]) -> Self {
        let mut key = 0u32;
        for (i, &pos) in combo.iter().enumerate() {
            key |= (tids[pos as usize] as u32) << (i * 8);
        }
        key
    }
}

impl PackKey for u64 {
    #[inline]
    fn pack(tids: &[u8], combo: &[u8]) -> Self {
        let mut key = 0u64;
        for (i, &pos) in combo.iter().enumerate() {
            key |= (tids[pos as usize] as u64) << (i * 8);
        }
        key
    }
}

fn find_intersections_packed<K: PackKey>(
    k: usize,
    post_tids: &[Vec<u8>],
    combo_table: &[Vec<Vec<Vec<u8>>>],
) -> Vec<Vec<PostIdx>> {
    let mut map: HashMap<K, Vec<PostIdx>> = HashMap::new();

    for (post_idx, tids) in post_tids.iter().enumerate() {
        if tids.len() < k {
            continue;
        }
        let combos = &combo_table[tids.len()][k];
        for combo in combos {
            map.entry(K::pack(tids, combo))
                .or_default()
                .push(post_idx as PostIdx);
        }
    }

    // Collect only groups with >= 2 posts
    map.into_values().filter(|list| list.len() >= 2).collect()
}

fn disseminate(
    intersection_lists: &[Vec<PostIdx>],
    trackers: &mut [TopNTracker],
    top_ns: &mut [[PostIdx; NUM_TOP_ITEMS]],
) {
    for post_indices in intersection_lists {
        for &i in post_indices {
            let i_usize = i as usize;
            let resolved = trackers[i_usize].resolved as usize;

            // Skip if this post already has NUM_TOP_ITEMS resolved entries
            if resolved >= NUM_TOP_ITEMS {
                continue;
            }

            let mut total = trackers[i_usize].total as usize;

            for &j in post_indices {
                if i == j {
                    continue;
                }

                // Check for duplicates in entries[0..total]
                let mut is_dup = false;
                for pos in 0..total {
                    if top_ns[i_usize][pos] == j {
                        is_dup = true;
                        break;
                    }
                }
                if is_dup {
                    continue;
                }

                if total < NUM_TOP_ITEMS {
                    // Room available: append
                    top_ns[i_usize][total] = j;
                    total += 1;
                    trackers[i_usize].total = total as u8;
                } else {
                    // Full: post_indices are sorted ascending (posts added in index order),
                    // so if j > last entry, all subsequent j will also be too large
                    if j > top_ns[i_usize][NUM_TOP_ITEMS - 1] {
                        break;
                    }
                    top_ns[i_usize][NUM_TOP_ITEMS - 1] = j;
                }

                // Insertion sort within the unresolved portion (entries[resolved..total])
                let sortable = &mut top_ns[i_usize][resolved..total];
                if !sortable.is_empty() {
                    let last = sortable.len() - 1;
                    let val = sortable[last];
                    let mut pos = last;
                    while pos > 0 && sortable[pos - 1] > val {
                        sortable[pos] = sortable[pos - 1];
                        pos -= 1;
                    }
                    sortable[pos] = val;
                }
            }
        }
    }
}

/// Fallback: standard algorithm with dirty-list tracking for cases where
/// k_max > 8 or too many unique tags for the powerset approach.
fn fallback_standard<'a>(posts: &'a [Post]) -> Vec<RelatedPosts<'a>> {
    let mut post_tags_map = HashMap::<&str, Vec<u32>>::with_capacity(128);
    for (post_idx, post) in posts.iter().enumerate() {
        for &tag in &post.tags {
            post_tags_map
                .entry(tag)
                .or_insert_with(|| Vec::with_capacity(1024))
                .push(post_idx as u32);
        }
    }

    let mut tagged_post_count = vec![0u8; posts.len()];
    let mut dirty: Vec<u32> = Vec::with_capacity(posts.len());

    posts
        .iter()
        .enumerate()
        .map(|(post_idx, post)| {
            for &tag in &post.tags {
                for &other_post_idx in &post_tags_map[tag] {
                    let idx = other_post_idx as usize;
                    if tagged_post_count[idx] == 0 {
                        dirty.push(other_post_idx);
                    }
                    tagged_post_count[idx] += 1;
                }
            }
            tagged_post_count[post_idx] = 0;

            let mut topk = [(0u8, 0u32); NUM_TOP_ITEMS];
            let mut min_tags = 0u8;
            for &idx in &dirty {
                let count = tagged_post_count[idx as usize];
                if count > min_tags {
                    topk[NUM_TOP_ITEMS - 1] = (count, idx);
                    for i in (0..NUM_TOP_ITEMS - 1).rev() {
                        if topk[i].0 >= count {
                            break;
                        }
                        topk.swap(i, i + 1);
                    }
                    min_tags = topk[NUM_TOP_ITEMS - 1].0;
                }
            }

            for &idx in &dirty {
                tagged_post_count[idx as usize] = 0;
            }
            dirty.clear();

            RelatedPosts {
                id: post.id,
                tags: &post.tags,
                related: topk.map(|(_, index)| &posts[index as usize]),
            }
        })
        .collect()
}
