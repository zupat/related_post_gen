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

### Updated Results from github workflow ([raw data](https://github.com/jinyus/related_post_gen/blob/main/results))

##### VM Specs ( AWS c7a.xlarge-4vCPU-8GB-RAM-Ubuntu 24.04 )

| Language         | Time (5k posts)                       | 20k posts                              | 60k posts                           | Total     |
| ---------------- | ------------------------------------- | -------------------------------------- | ----------------------------------- | --------- |
| _D HO_[^1] | 10.77 ms | 40.07 ms | 113.05 ms | 163.89 ms |
| _Julia HO_[^1] | 5.90 ms | 108.67 ms | 80.67 ms | 195.23 ms |
| D (v2) | 12.06 ms | $\textsf{\color{lightgreen}122.21 ms}$ | $\textsf{\color{lightgreen}961.08 ms}$ | 1.10 s |
| Rust | $\textsf{\color{lightgreen}9.84 ms}$ | 124.59 ms | 1.10 s | 1.23 s |
| c3 | 10.50 ms | 141.00 ms | 1.13 s | 1.28 s |
| Zig | 15.00 ms | 199.33 ms | 1.71 s | 1.93 s |
| C++ | 15.00 ms | 204.33 ms | 1.76 s | 1.98 s |
| Odin | 14.87 ms | 208.01 ms | 1.78 s | 2.00 s |
| Go | 16.64 ms | 240.66 ms | 2.12 s | 2.37 s |
| Java (JIT) | 20.80 ms | 246.00 ms | 2.12 s | 2.39 s |
| Neat | 19.01 ms | 254.22 ms | 2.15 s | 2.42 s |
| Haskell | 21.10 ms | 277.44 ms | 2.24 s | 2.54 s |
| Nim | 18.23 ms | 259.83 ms | 2.29 s | 2.57 s |
| F# (AOT) | 18.68 ms | 266.89 ms | 2.33 s | 2.61 s |
| C# (JIT) | 21.20 ms | 296.19 ms | 2.48 s | 2.80 s |
| D | 22.64 ms | 297.77 ms | 2.56 s | 2.88 s |
| Julia | 19.51 ms | 296.29 ms | 2.58 s | 2.90 s |
| Vlang | 22.08 ms | 318.69 ms | 2.76 s | 3.10 s |
| C# (AOT) | 21.21 ms | 320.34 ms | 2.99 s | 3.34 s |
| Swift | 28.94 ms | 384.12 ms | 3.32 s | 3.74 s |
| F# (JIT) | 27.57 ms | 377.57 ms | 3.46 s | 3.87 s |
| Java (GraalVM) | 34.30 ms | 422.33 ms | 3.64 s | 4.10 s |
| Crystal | 38.90 ms | 577.98 ms | 5.12 s | 5.74 s |
| JS (Bun) | 60.80 ms | 743.67 ms | 6.30 s | 7.10 s |
| LuaJIT | 63.19 ms | 801.74 ms | 6.56 s | 7.42 s |
| Pypy | 59.74 ms | 700.40 ms | 6.68 s | 7.44 s |
| Numba | 62.86 ms | 803.85 ms | 6.81 s | 7.68 s |
| Dart VM | 57.50 ms | 885.00 ms | 7.11 s | 8.05 s |
| JS (Node) | 80.40 ms | 913.33 ms | 7.77 s | 8.77 s |
| Dart AOT | 59.30 ms | 900.67 ms | 7.93 s | 8.89 s |
| Common Lisp (SBCL) | 130.00 ms | 985.00 ms | 8.15 s | 9.27 s |
| Clojure | 95.60 ms | 1.10 s | 9.27 s | 10.47 s |
| JS (Deno) | 84.90 ms | 1.15 s | 10.50 s | 11.74 s |
| Ocaml | 84.60 ms | 1.30 s | 11.58 s | 12.96 s |
| Racket | 114.55 ms | 1.71 s | 13.91 s | 15.74 s |
| Typed Racket | 117.17 ms | 1.77 s | 13.97 s | 15.85 s |
| Scala Native | 260.20 ms | 3.08 s | 25.91 s | 29.25 s |
| Lobster (JIT) | 468.85 ms | 7.17 s | 63.88 s | 71.52 s |
| LuaJIT (JIT OFF) | 543.40 ms | 8.23 s | 82.45 s | 91.22 s |
| Erlang | 647.18 ms | 10.30 s | 90.75 s | 101.70 s |
| PHP | 611.30 ms | 9.80 s | 92.21 s | 102.62 s |
| Lua | 837.32 ms | 12.76 s | 116.07 s | 129.66 s |
| Python | 1.07 s | 16.67 s | 147.84 s | 165.58 s |
| Ruby | 1.49 s | 24.06 s | 206.08 s | 231.63 s |

### Multicore Results

| Language       | Time (5k posts) | 20k posts        | 60k posts        | Total     |
| -------------- | --------------- | ---------------- | ---------------- | --------- |
| D Concurrent (v2) | 6.22 ms | 46.71 ms | $\textsf{\color{lightgreen}273.33 ms}$ | 326.26 ms |
| D Concurrent | 6.65 ms | 50.79 ms | 345.70 ms | 403.15 ms |
| C++ Concurrent | $\textsf{\color{lightgreen}4.00 ms}$ | $\textsf{\color{lightgreen}46.33 ms}$ | 379.67 ms | 430.00 ms |
| C# Concurrent (JIT) | 5.70 ms | 54.21 ms | 403.47 ms | 463.38 ms |
| C# Concurrent (AOT) | 4.35 ms | 53.92 ms | 442.09 ms | 500.36 ms |
| Nim Concurrent | 4.60 ms | 59.70 ms | 505.32 ms | 569.63 ms |
| Zig Concurrent | 6.19 ms | 68.64 ms | 550.06 ms | 624.89 ms |
| F# Concurrent | 5.50 ms | 75.00 ms | 604.33 ms | 684.83 ms |
| Rust Concurrent | 5.54 ms | 73.28 ms | 637.51 ms | 716.33 ms |
| Julia Concurrent | 6.82 ms | 79.38 ms | 679.22 ms | 765.42 ms |
| Go Concurrent | 7.02 ms | 90.35 ms | 785.71 ms | 883.09 ms |
| F# Concurrent (AOT) | 8.00 ms | 121.00 ms | 1.08 s | 1.21 s |
| Swift Concurrent | 12.08 ms | 131.59 ms | 1.09 s | 1.24 s |
| Java Concurrent (JIT) | 63.00 ms | 191.33 ms | 1.16 s | 1.41 s |
| Numba Concurrent | 20.75 ms | 181.30 ms | 1.36 s | 1.56 s |
| Java (GraalVM) Concurrent | 12.00 ms | 209.33 ms | 1.51 s | 1.73 s |

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