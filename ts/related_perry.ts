export interface Post {
    _id: string;
    title: string;
    tags: string[];
}

export interface RelatedPost {
    _id: string;
    title: string;
    tags: string[];
    related: Post[];
}

// Perry-specific variant of related.ts. The algorithm is identical; only the
// representations differ, because Perry lowers typed-array access differently:
//
//   * counts live in a Uint32Array and are bumped with `+= 1`, which hits
//     Perry's guarded direct read-modify-write path for Uint32Array elements.
//     `++` on an Int32Array falls back to the generic get/add/set path and is
//     roughly 8x slower on this workload.
//   * each tag's post list is an Int32Array, so the index read `others[k]`
//     stays unboxed instead of coming out of a `number[]`.
//
// Both are plain TypeScript and only depend on the standard typed arrays.
export function genRelatedPosts(posts: Post[]): RelatedPost[] {
    const postsCount = posts.length;

    // Tags become dense integer ids so the hot loop never hashes a string.
    const tagIds = new Map<string, number>();
    const tagPostLists: number[][] = [];

    for (let i = 0; i < postsCount; i++) {
        const tags = posts[i].tags;
        for (let j = 0; j < tags.length; j++) {
            const tag = tags[j];
            let id = tagIds.get(tag);
            if (id === undefined) {
                id = tagPostLists.length;
                tagIds.set(tag, id);
                tagPostLists.push([]);
            }
            tagPostLists[id].push(i);
        }
    }

    const tagPosts: Int32Array[] = [];
    for (let t = 0; t < tagPostLists.length; t++) {
        tagPosts.push(Int32Array.from(tagPostLists[t]));
    }

    const taggedPostCount = new Uint32Array(postsCount);
    const allRelated = new Array<RelatedPost>(postsCount);

    const topIndices = new Uint32Array(5);
    const topCounts = new Uint32Array(5);

    for (let i = 0; i < postsCount; i++) {
        taggedPostCount.fill(0);

        const post = posts[i];
        const tags = post.tags;

        for (let j = 0; j < tags.length; j++) {
            const id = tagIds.get(tags[j]);
            if (id === undefined) {
                continue;
            }
            const others = tagPosts[id];
            for (let k = 0; k < others.length; k++) {
                taggedPostCount[others[k]] += 1;
            }
        }

        taggedPostCount[i] = 0; // a post is not related to itself

        // Keep the five highest counts in two fixed-size arrays. The list is
        // short, so insertion is cheaper than a heap here.
        topCounts.fill(0);
        let filled = 0;
        let minTags = 0;

        for (let j = 0; j < postsCount; j++) {
            const count = taggedPostCount[j];
            if (count <= minTags) {
                continue;
            }

            let pos = 4;
            while (pos >= 0 && count > topCounts[pos]) {
                pos--;
            }
            pos++;

            for (let k = 4; k > pos; k--) {
                topCounts[k] = topCounts[k - 1];
                topIndices[k] = topIndices[k - 1];
            }
            topCounts[pos] = count;
            topIndices[pos] = j;

            if (filled < 5) {
                filled++;
            }
            minTags = filled === 5 ? topCounts[4] : 0;
        }

        const related: Post[] = [];
        for (let j = 0; j < filled; j++) {
            related.push(posts[topIndices[j]]);
        }

        allRelated[i] = {
            _id: post._id,
            title: post.title,
            tags: post.tags,
            related: related,
        };
    }

    return allRelated;
}
