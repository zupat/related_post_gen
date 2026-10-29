import { readFileSync, writeFileSync } from "node:fs";
import { genRelatedPosts } from "./related";
import type { Post } from "./related";

const posts = JSON.parse(readFileSync("../posts.json", "utf8")) as Post[];

const start = Date.now();

const allRelatedPosts = genRelatedPosts(posts);

console.log(`Processing time (w/o IO): ${Date.now() - start}ms`);

writeFileSync(
    "../related_posts_perry.json",
    JSON.stringify(allRelatedPosts)
);
