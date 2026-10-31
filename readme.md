### Problem:

Given a list of posts, compute the top 5 related posts for each post based on the number of shared tags.

<details>
<summary> Steps </summary>

-   Read the posts JSON file.
-   Iterate over the posts and populate a map containing: `tag -> List<int>`, with the int representing the post index of each post with that tag.
-   Iterate over the posts and for each post:
    -   Create a map: `PostIndex -> int` to track the number of shared tags
    -   For each tag, Iterate over the posts that have that tag
    -   For each post, increment the shared tag count in the map.
-   Sort the related posts by the number of shared tags.
-   Write the top 5 related posts for each post to a new JSON file.
</details>

### Run Benchmark

See [setup.md](setup.md) for complete guide.

```bash
./run.sh go | rust | python | all

# windows (powershell)
./run.ps1 go | rust | python | all
# OR
pwsh ./run.ps1 go | rust | python | all

# Docker (check the dockerfiles/base.Dockerfile for available variables)
./gen_dockerfile.sh -b go | rust | python | all
# THEN

./docker_run.sh go | rust | python | all
# OR use the image directly
docker run -e TEST_NAME=go -it --rm go_databench
```

<details>
<summary> Rules </summary>

<h3>No:</h3>

-   FFI (including assembly inlining)
-   Unsafe code blocks
-   Custom benchmarking
-   Disabling runtime checks (bounds etc)
-   Specific hardware targeting
-   SIMD for single threaded solutions
-   Hardcoding number of posts
-   Lazy evaluation (Unless results are computed at runtime and timed)
-   Computation Caching

<h3>Must:</h3>

-   Support up to 100,000 posts
-   Support UTF8 strings
-   Parse json at runtime
-   Support up to 100 tags
-   Represent tags as strings
-   Be production ready
-   Use less than 8GB of memory
</details>

### Single Core Results ([raw data](https://github.com/jinyus/related_post_gen/blob/main/results))

##### VM Specs ( OVHcloud bm-s1-8vCPU-32GB-RAM-Ubuntu 26.04 )

| Language         | Time (5k posts)                       | 20k posts                              | 60k posts                           | Total     |
| ---------------- | ------------------------------------- | -------------------------------------- | ----------------------------------- | --------- |
| D (v2) | $\textsf{\color{lightgreen}13.38 ms}$ | $\textsf{\color{lightgreen}153.50 ms}$ | $\textsf{\color{lightgreen}1.20 s}$ | 1.36 s |
| c3 | 14.10 ms | 171.67 ms | 1.33 s | 1.52 s |
| Rust | 13.56 ms | 172.45 ms | 1.40 s | 1.58 s |
| Zig | 14.80 ms | 179.33 ms | 1.43 s | 1.63 s |
| C++ | 14.20 ms | 177.33 ms | 1.46 s | 1.65 s |
| Odin | 17.78 ms | 229.20 ms | 1.93 s | 2.17 s |
| D | 18.76 ms | 236.75 ms | 1.98 s | 2.24 s |
| Nim | 17.72 ms | 234.58 ms | 2.00 s | 2.25 s |
| Go | 17.95 ms | 245.89 ms | 2.11 s | 2.37 s |
| Neat | 21.99 ms | 260.59 ms | 2.16 s | 2.45 s |
| C# (JIT) | 20.89 ms | 275.58 ms | 2.17 s | 2.47 s |
| Fortran | 14.30 ms | 230.00 ms | 2.29 s | 2.54 s |
| C# (AOT) | 29.63 ms | 282.96 ms | 2.43 s | 2.74 s |
| F# (JIT) | 23.85 ms | 346.59 ms | 3.05 s | 3.42 s |
| Julia | 21.53 ms | 334.98 ms | 3.10 s | 3.46 s |
| Java (JIT) | 28.00 ms | 378.33 ms | 3.31 s | 3.71 s |
| Java (GraalVM) | 23.90 ms | 357.33 ms | 3.37 s | 3.75 s |
| Haskell | 34.04 ms | 437.22 ms | 3.71 s | 4.18 s |
| F# (AOT) | 27.93 ms | 413.88 ms | 3.79 s | 4.24 s |
| Vlang | 30.87 ms | 427.41 ms | 3.83 s | 4.29 s |
| Swift | 27.57 ms | 362.25 ms | 4.06 s | 4.45 s |
| Common Lisp (SBCL) | 36.00 ms | 470.00 ms | 4.11 s | 4.62 s |
| Numba | 48.91 ms | 576.45 ms | 4.55 s | 5.18 s |
| Scala Native | 64.10 ms | 651.67 ms | 5.16 s | 5.87 s |
| Hare | 42.39 ms | 614.00 ms | 5.47 s | 6.13 s |
| Crystal | 47.81 ms | 658.87 ms | 5.71 s | 6.42 s |
| TypeScript (scriptc) | 50.70 ms | 725.67 ms | 6.44 s | 7.21 s |
| Ada | 59.08 ms | 802.16 ms | 6.90 s | 7.76 s |
| Dart VM | 63.90 ms | 751.67 ms | 7.66 s | 8.48 s |
| LuaJIT | 85.55 ms | 931.89 ms | 8.28 s | 9.30 s |
| Haxe | 66.20 ms | 961.33 ms | 8.42 s | 9.45 s |
| JS (Bun) | 76.30 ms | 960.00 ms | 8.74 s | 9.78 s |
| Dart AOT | 70.70 ms | 1.05 s | 9.28 s | 10.40 s |
| JS (Node) | 99.00 ms | 1.23 s | 10.26 s | 11.59 s |
| JS (Deno) | 93.90 ms | 1.18 s | 10.57 s | 11.85 s |
| Pypy | 86.47 ms | 1.18 s | 10.83 s | 12.09 s |
| Clojure | 94.30 ms | 1.08 s | 10.98 s | 12.15 s |
| Ocaml | 90.10 ms | 1.33 s | 11.86 s | 13.28 s |
| Flix | 127.60 ms | 1.51 s | 13.21 s | 14.85 s |
| Nax | 151.30 ms | 2.07 s | 18.06 s | 20.28 s |
| Lobster (JIT) | 152.55 ms | 2.28 s | 20.55 s | 22.99 s |
| Racket | 166.72 ms | 2.51 s | 21.78 s | 24.46 s |
| Typed Racket | 167.70 ms | 2.46 s | 22.34 s | 24.97 s |
| TypeScript (Perry) | 449.50 ms | 6.27 s | 54.71 s | 61.43 s |
| LuaJIT (JIT OFF) | 470.18 ms | 7.23 s | 64.28 s | 71.98 s |
| PHP | 597.30 ms | 9.35 s | 83.54 s | 93.49 s |
| Erlang | 616.03 ms | 9.87 s | 101.74 s | 112.23 s |
| Python | 1.10 s | 17.94 s | 160.21 s | 179.24 s |
| Gleam | 793.95 ms | 12.61 s | 173.79 s | 187.19 s |
| Ruby | 1.18 s | 18.62 s | 170.60 s | 190.41 s |
| Lua | 1.33 s | 20.73 s | 184.99 s | 207.05 s |

### Multicore Results

| Language       | Time (5k posts) | 20k posts        | 60k posts        | Total     |
| -------------- | --------------- | ---------------- | ---------------- | --------- | 
| Rust Concurrent | $\textsf{\color{lightgreen}3.99 ms}$ | 49.60 ms | $\textsf{\color{lightgreen}382.57 ms}$ | 436.16 ms |
| D Concurrent | 5.79 ms | 52.34 ms | 398.19 ms | 456.32 ms |
| C++ Concurrent | 5.00 ms | 58.67 ms | 437.33 ms | 501.00 ms |
| C# Concurrent (JIT) | 5.47 ms | 60.07 ms | 437.68 ms | 503.22 ms |
| D Concurrent (v2) | 5.26 ms | $\textsf{\color{lightgreen}47.11 ms}$ | 464.53 ms | 516.90 ms |
| Zig Concurrent | 5.74 ms | 61.64 ms | 499.30 ms | 566.67 ms |
| C# Concurrent (AOT) | 5.45 ms | 69.50 ms | 530.30 ms | 605.25 ms |
| F# Concurrent | 5.30 ms | 71.00 ms | 606.00 ms | 682.30 ms |
| Go Concurrent | 5.73 ms | 80.98 ms | 689.35 ms | 776.06 ms |
| Julia Concurrent | 7.20 ms | 86.92 ms | 839.45 ms | 933.57 ms |
| Swift Concurrent | 10.43 ms | 115.01 ms | 1.03 s | 1.16 s |
| Nim Concurrent | 8.71 ms | 118.79 ms | 1.04 s | 1.17 s |
| Java (GraalVM) Concurrent | 10.20 ms | 144.33 ms | 1.17 s | 1.33 s |
| F# Concurrent (AOT) | 9.50 ms | 147.00 ms | 1.23 s | 1.39 s |
| Numba Concurrent | 27.80 ms | 193.65 ms | 1.26 s | 1.48 s |
| Java Concurrent (JIT) | 131.80 ms | 260.00 ms | 1.25 s | 1.64 s |

### Highly Optimised Results

| Language       | Time (5k posts) | 20k posts        | 60k posts        | Total     |
| -------------- | --------------- | ---------------- | ---------------- | --------- | 
| _Rust HO_[^1] | $\textsf{\color{lightgreen}4.27 ms}$ | $\textsf{\color{lightgreen}13.72 ms}$ | $\textsf{\color{lightgreen}34.78 ms}$ | 52.77 ms |
| _Go HO_[^1] | 4.50 ms | 15.48 ms | 37.76 ms | 57.74 ms |
| _Julia HO_[^1] | 4.50 ms | 34.33 ms | 73.67 ms | 112.50 ms |
| _D HO_[^1] | 9.69 ms | 38.25 ms | 97.45 ms | 145.39 ms |

> **Disclaimer:** These results are not directly comparable to the other implementations. The HO entries hardcode the top-5 output (`NUM_TOP_ITEMS`/`TopN`/`SVector{5}`) and exploit constraints of the benchmark dataset that don't generalize:
> - **Rust HO** uses 8-bit tag IDs, so it fails with more than 256 unique tags, and posts with more than 8 tags fall back to the standard, unoptimized algorithm.
> - **D HO** requires posts to have at most 8 tags (it aborts otherwise) and uses lookup tables limited to 256 entries.
> - **Julia HO** always returns exactly 5 related posts via a specialized data structure.
> - **Go HO** requires posts to have at most 8 tags and the dataset to have at most 256 unique tags (it exits with an error otherwise), and always returns exactly 5 related posts.
>
> They only produce these speeds for this specific dataset/shape of input; an arbitrary number of tags would break them or require changes.

<details>
<summary> Old Results with details (on my machine) </summary>

| Language   | Processing Time | Total (+ I/O) | Details                                                                                                                                                                                                                                                                                         |
| ---------- | --------------- | ------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Rust       | -               | 4.5s          | Initial                                                                                                                                                                                                                                                                                         |
| Rust v2    | -               | 2.60s         | Replace std HashMap with fxHashMap by [phazer99](https://www.reddit.com/r/rust/comments/16plgok/comment/k1rtr4x/?utm_source=share&utm_medium=web2x&context=3)                                                                                                                                   |
| Rust v3    | -               | 1.28s         | Preallocate and reuse map and unstable sort by [vdrmn](https://www.reddit.com/r/rust/comments/16plgok/comment/k1rzo7g/?utm_source=share&utm_medium=web2x&context=3) and [Darksonn](https://www.reddit.com/r/rust/comments/16plgok/comment/k1rzwdx/?utm_source=share&utm_medium=web2x&context=3) |
| Rust v4    | -               | 0.13s         | Use Post index as key instead of Pointer and Binary Heap by [RB5009](https://www.reddit.com/r/rust/comments/16plgok/comment/k1s5ea0/?utm_source=share&utm_medium=web2x&context=3)                                                                                                               |
| Rust v5    | 38ms            | 52ms          | Rm hashing from loop and use vec[count] instead of map[index]count by RB5009                                                                                                                                                                                                                    |
| Rust v6    | 23ms            | 36ms          | Optimized Binary Heap Ops by [scottlamb](https://github.com/jinyus/related_post_gen/pull/12)                                                                                                                                                                                                    |
| Rust Rayon | 9ms             | 22ms          | Parallelize by [masmullin2000](https://github.com/jinyus/related_post_gen/pull/4)                                                                                                                                                                                                               |
| Rust Rayon | 8ms             | 22ms          | Remove comparison out of hot loop                                                                                                                                                                                                                                                               |
| ⠀          | ⠀               | ⠀             | ⠀                                                                                                                                                                                                                                                                                               |
| Go         | -               | 1.5s          | Initial                                                                                                                                                                                                                                                                                         |
| Go v2      | -               | 80ms          | Add rust optimizations                                                                                                                                                                                                                                                                          |
| Go v3      | 56ms            | 70ms          | Use goccy/go-json                                                                                                                                                                                                                                                                               |
| Go v3      | 34ms            | 55ms          | Use generic binaryheap by [DrBlury](https://github.com/jinyus/related_post_gen/pull/7)                                                                                                                                                                                                          |
| Go v4      | 26ms            | 50ms          | Replace binary heap with custom priority queue                                                                                                                                                                                                                                                  |
| Go v5      | 20ms            | 43ms          | Remove comparison out of hot loop                                                                                                                                                                                                                                                               |
| Go Con     | 10ms            | 33ms          | Go concurrency by [tirprox](https://github.com/jinyus/related_post_gen/pull/17) and [DrBlury](https://github.com/jinyus/related_post_gen/pull/8)                                                                                                                                                |
| Go Con v2  | 5ms             | 29ms          | Use arena, use waitgroup, rm binheap by [DrBlury](https://github.com/jinyus/related_post_gen/pull/20)                                                                                                                                                                                           |
| ⠀          | ⠀               | ⠀             | ⠀                                                                                                                                                                                                                                                                                               |
| Python     | -               | 7.81s         | Initial                                                                                                                                                                                                                                                                                         |
| Python v2  | 1.35s           | 1.53s         | Add rust optimizations by [dave-andersen](https://github.com/jinyus/related_post_gen/pull/10)                                                                                                                                                                                                   |
| Numpy      | 0.57s           | 0.85s         | Numpy implementation by [Copper280z](https://github.com/jinyus/related_post_gen/pull/11)                                                                                                                                                                                                        |
| ⠀          | ⠀               | ⠀             | ⠀                                                                                                                                                                                                                                                                                               |
| Crystal    | 50ms            | 96ms          | Inital w/ previous optimizations                                                                                                                                                                                                                                                                |
| Crystal v2 | 33ms            | 72ms          | Replace binary heap with custom priority queue                                                                                                                                                                                                                                                  |
| ⠀          | ⠀               | ⠀             | ⠀                                                                                                                                                                                                                                                                                               |
| Odin       | 110ms           | 397ms         | Ported from golang code                                                                                                                                                                                                                                                                         |
| Odin v2    | 104ms           | 404ms         | Remove comparison out of hot loop                                                                                                                                                                                                                                                               |
| ⠀          | ⠀               | ⠀             | ⠀                                                                                                                                                                                                                                                                                               |
| Dart VM    | 125ms           | 530ms         | Ported from golang code                                                                                                                                                                                                                                                                         |
| Dart bin   | 274ms           | 360ms         | Compiled executable                                                                                                                                                                                                                                                                             |
| ⠀          | ⠀               | ⠀             | ⠀                                                                                                                                                                                                                                                                                               |
| Vlang      | 339ms           | 560ms         | Ported from golang code                                                                                                                                                                                                                                                                         |
| ⠀          | ⠀               | ⠀             | ⠀                                                                                                                                                                                                                                                                                               |
| Zig        | 80ms            | 110ms         | Provided by [akhildevelops](https://github.com/jinyus/related_post_gen/pull/30)                                                                                                                                                                                                                 |

</details>

[^1]: Uses specialized datastructures meant for demonstration purposes: [more](https://github.com/LilithHafner/Jokes/tree/main/SuperDataStructures.jl)

[^2]: Inko is currently in beta and optimizations haven't been applied. [more](https://github.com/jinyus/related_post_gen/pull/440#issuecomment-1816583612)