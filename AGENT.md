# AGENT.md — how this project works

Orientation for agents working in this repo. It is a **cross-language benchmark**:
for every post in `posts.json`, compute the top 5 related posts by number of shared
tags. Each language has an implementation; the timings are published in
`readme.md` and the raw runs live in `results/`. The tables in `readme.md` are the
product — everything else exists to produce them.

If you only read three files, read `run.sh` (how a language is executed),
`docker_start.sh` (how a test container runs the three datasets) and
`update_results.sh` (how results become a PR).

---

## 1. Data flow

```
gen_fake_posts.py N        →  posts.json (5k / 20k / 60k posts, 28 fixed tags, 4-5 per post)
<language>/                →  related_posts_<lang>.json   (top 5 per post)
verify.py                  →  compares against correct_related.json
                              (sum of shared-tag counts over the 5 related posts)
raw_results_<test>.md      →  one file per test, three sections (5k, 20k, 60k),
                              each holding the "Processing time (w/o IO)" lines
results/                   →  all raw_results_*.md (committed; the raw data linked from the readme)
extract_results.dart       →  rewrites the three tables in readme.md
update_results.sh          →  orchestrates all of the above in docker images and opens the PR
```

Published numbers are machine-specific: `readme.md` carries a
`##### VM Specs ( <label> )` line that must describe the machine that ran them.

---

## 2. Repo map

| Path | What it is |
|---|---|
| `posts.json`, `correct_related.json` | 5k-post input and the expected shared-tag sums |
| `<lang>/` … | one directory per implementation (go, rust, cpp, java, fsharp, …) |
| `run.sh` | test driver: builds and runs one implementation, writes the results section |
| `verify.py` | correctness check (shared-tag sum over the top 5) |
| `gen_fake_posts.py` | dataset generator: `python gen_fake_posts.py 20000` writes `posts.json` |
| `dockerfiles/<image>.Dockerfile` | one image per runtime family — **self-contained**, each starts with `FROM` |
| `docker_run.sh` | test-name → image map (`NameToImage`) and a manual runner |
| `gen_dockerfile.sh` | builds an image (`-b <name>`) from its fragment |
| `docker_start.sh` | container entrypoint: clone repo → run 5k/20k/60k → copy results to `/results` |
| `update_results.sh` | the orchestrator: images → tests → extraction → commit → PR |
| `extract_results.dart` | parser that rewrites the readme tables from `results/` |
| `results/raw_results_<test>.md` | committed raw results (input to the extractor) |
| `readme.md` | results tables + rules + HO disclaimer |
| `ovhcloud_bench.sh` | one-command provider: create a Metal Instance, set it up, run in tmux |
| `ghcr_images.sh` | publish/pull the built images (GitHub container registry) |
| `.agents/skills/update-toolchains/` | user-invoked skill: refresh stale toolchain pins, verify each image in Vercel Sandboxes, open one PR |
| `setup.md` | human setup guide (manual + wrappers) |
| `.github/workflows/build-images.yml` | `workflow_dispatch` matrix that builds every image in parallel (optionally pushes to GHCR) |
| `.github/workflows/benchmark.yml` | legacy native-benchmark workflow (not the current path) |

---

## 3. What an implementation must satisfy

Rules (also in `readme.md`): no FFI or unsafe code, no custom benchmarking, no
disabling runtime checks, no hardware-specific targeting, no SIMD in single-threaded
solutions, no hardcoding the post count, no lazy evaluation, no computation caching.
Must: support up to 100,000 posts and up to 100 tags, UTF-8 strings, parse JSON at
runtime, represent tags as strings, be "production ready", stay under 8 GB.

Output shape (`related_posts_<lang>.json`): array of
`{"_id", "tags", "related": [5 posts]}`, one entry per input post, same order.

Correctness is judged **only** by the sum of shared-tag counts across the 5 related
posts (`verify.py`), so tie-breaking and ordering inside `related` are free.

---

## 4. `run.sh` — the timing harness

```bash
./run.sh <test-name>              # stdout only, 10 runs
./run.sh <test-name> <outfile>    # writes a results file, 10 runs (3 for slow languages)
./run.sh <test-name> <outfile> append   # appends, 3 runs (2 for slow languages)
```

- Dispatches to `run_<test>()`, which `cd`s into the language directory, builds (first
  round only) and calls `run_command "<Title>" <runs> <binary>`.
- `run_command` runs the binary `runs` times through GNU `time`, prefixes each line with
  a tab and `tee -a`s it into `$outfile`. The **GNU `time` binary and `which` must exist**
  in the image (`run.sh` refuses to start otherwise).
- A run that takes longer than `LONG_RUN_THRESHOLD_SECONDS` (default **30**) is not
  repeated: the sample already recorded is kept and the remaining repeats are skipped.
  This keeps the minute-long 60k runs (PHP, Python, Ruby, Lua, …) from dominating a full
  pass. Set the variable to `0` to always repeat; `extract_results.dart` only needs one
  sample per dataset section, so the shorter sample count does not break extraction.
- **Exactly one `Processing time (w/o IO)` line per run** — `extract_results.dart`
  averages every matching line it sees in a section, so extra lines corrupt the average.
- `check_output` (`python3 verify.py <json>`) runs only in the first round and fails the
  test on a mismatch.
- `$outfile` is resolved to an **absolute path** at startup: runners may `cd` arbitrarily
  deep (`fsharp` goes two levels down). Do not reintroduce a relative `"../$outfile"`.
- Runners end with `check_output` **and** a `cd ..` convention: `run.sh` moves up one
  level per runner `cd`, and `check_output` moves up the last one.

Adding a language: create `<dir>/`, add `run_<name>()` + dispatch + (if it belongs in
`all`) the `all` list, add `["<test>"]="<image>"` to `docker_run.sh`, add
`dockerfiles/<image>.Dockerfile`, and make sure `./run.sh <test> out.md` produces
`related_posts_<lang>.json` + a valid results section.

---

## 5. Images (`dockerfiles/`)

Every fragment is **self-contained** (`FROM …` is the first instruction) and is used as
the whole Dockerfile by `gen_dockerfile.sh`; the old behaviour (merge into a shared
`base.Dockerfile`) is only a fallback. All Arch packages are gone.

- `base` is the Dart SDK image + clang/g++ and serves `cpp`, `cpp_con`, `dart`, `dart_aot`.
- Others are official language images (`golang:1.27.3-bookworm`, `rust:1.99.1-bookworm`,
  `julia:1.11-bookworm`, `swift:6.4.1-bookworm`, `dotnet/sdk:10.0.402`, `node:22-bookworm-slim`,
  `crystallang/crystal:1.22.0`, `python:3.12-slim`) or `debian:12-slim` + apt/pinned tarball
  (zig 0.17.0, odin dev-2026-10, nim 2.2.6, v 0.5.2, c3 0.8.5_1, neat 0.7.1, …).
- Each image must carry the **harness**: `git`, `python3` + `faker`/`nanoid`, GNU `time`,
  `which`, `curl`/`wget`, and (where a runner calls it) `sudo`. It must also end with the
  same tail: `WORKDIR /app`, `VOLUME /results`, the `GIT_*`/`RUN2`/`RUN3`/`TEST_NAME` env,
  `COPY docker_start.sh /docker_start.sh`, `CMD ["/docker_start.sh"]`.
- Builds happen **at run time** (`run.sh` compiles inside the container), so compilers,
  libc headers and linkers must stay in the image — multi-stage "slim runtime" is not
  applicable without changing the harness.
- Toolchains that broke on rolling releases are **pinned with a comment** (Zig 0.17.0 for
  the `std.Io` reader/writer API, nim 2.2.x for `nimble`, real clang-15/llvm-15 for neat,
  GHC 9.14 + llvm-15 (clang assembles llc output) for the `-fllvm` GHC2024 build).

`docker_run.sh` holds the authoritative `test → image` map (`NameToImage`); `update_results.sh`
parses it, so keep the `["name"]="image"` formatting.

### Verifying an image change — in a Vercel Sandbox, never locally

Do **not** build or run these images on the machine the agent runs on. They are
large, the timings are only meaningful on a clean box, and a local build competes
with everything else for CPU/disk. Use a Vercel Sandbox:

```bash
.agents/skills/update-toolchains/scripts/check-sandbox.sh   # 0 = ready, 2 = missing, 3 = not logged in
.agents/skills/update-toolchains/scripts/vercel-run.sh \
  --worktree <worktree> \
  --dockerfile dockerfiles/<image>.Dockerfile \
  --tag <image>_test <test> [test...]
```

`vercel-run.sh` creates the sandbox, installs and starts Docker inside it, copies
the worktree in, and runs `scripts/run-tests.sh`, which builds a copy of the
Dockerfile with the `#precompile` block stripped (that block clones `main` and
would test old sources) and runs each test with `bash -c "./run.sh <test>"` — not
`-lc`, which sources `/etc/profile` and drops the toolchain from `PATH`. A test
passes only when its output shows `Success: … is valid!` or a zero exit code.

Local `run-tests.sh --engine docker|podman` exists as a fallback **only** after the
user explicitly agrees; `vercel-run.sh` refuses to fall back on its own. Full
details: `.agents/skills/update-toolchains/SKILL.md` and its
`references/sandbox-recipe.md`.

When several sandboxes run at once, pass an explicit `--name` to each. The
default name is `<worktree basename>-<epoch seconds>`, so two runs started from
the same worktree in the same second collide: one dies and the other's `exec`
fails with `Named sandbox … not found`.

---

## 6. Test containers (`docker_start.sh`)

Runs inside the image: clones `$GIT_REPO` (default `https://github.com/zupat/related_post_gen.git`)
at `$BRANCH`, then

```
./run.sh $TEST_NAME raw_results_$TEST_NAME.md          # 5k
python gen_fake_posts.py $RUN2 && ./run.sh … append    # 20k
python gen_fake_posts.py $RUN3 && ./run.sh … append    # 60k
cp raw_results_$TEST_NAME.md /results/
```

Consequences that matter:

- **The benchmarked code is the pushed commit**, not the local working tree.
- A test that fails keeps no results file at all, so failures are clean (`cp` never runs).
- `/results` is a mounted directory (on the VM: `results/`).

---

## 7. `extract_results.dart`

Input: a results file or a directory of `*.md` (dir mode is what `update_results.sh` uses).

- A line matching `^[a-zA-Z]` starts a new score; its name is the line with `:`/newlines
  stripped (`Go HO:` → `Go HO`). Every `Processing time (w/o IO)` match inside a section is
  averaged (units `ms`/`s`/`milliseconds`).
- **Each language needs exactly three sections (5k, 20k, 60k)**; otherwise it prints
  "Enough scores not found…" and returns without touching the readme. The guard only
  checks non-concurrent, non-HO scores, but `toRowString` indexes two more sections for
  those as well — so give every language three.
- Routing: names containing `Concurrent` → multicore table; `hoNames`
  (`Julia HO`, `D HO`, `Rust HO`, `Go HO`) → HO table; everything else → single-core.
  Display names are remapped (`_Go HO_[^1]`, `Inko[^2]`).
- Fastest per column gets a green `$\textsf{\color{lightgreen}…}$`; `OOM` is used when a
  language has no times.
- Lines between the first table separator and `<details>` are **replaced**. Blockquote
  lines (the HO disclaimer) are preserved and re-emitted after the HO table — keep that
  behaviour or the disclaimer disappears on the next extraction.

---

## 8. `update_results.sh` — the orchestrator

Preflight: docker usable (`sudo` fallback), gh installed/authenticated (skipped with
`--no-pr`), dart on the host or in `base_databench`, and `HEAD == origin/<branch>`
(the containers clone the remote). Then:

1. **Images**: builds each needed image (`gen_dockerfile.sh -b`), or pulls it from GHCR
   with `--pull-images`. A failed build is **non-fatal**: its tests are skipped and listed
   (`--fail-fast` aborts instead).
2. **Tests**: serial loop in `docker_run.sh` map order, one container at a time, results
   into `results/`, failures recorded and the run continues.
   Resume: `--skip-existing` skips tests whose result was produced **in this campaign**
   (`results/.campaign-ignore` lists the results that pre-existed, i.e. shipped in the
   repo; each passing test is removed from it). `--new-campaign` resets it.
3. **Extraction** over `results/` → `readme.md`.
4. **Publish**: parks `readme.md` + `results/`, `git checkout -f` onto the results branch
   (reusing it if it exists), restores the fresh files, commits (identity from git config
   or `gh api user`, overridable with `GH_COMMIT_NAME`/`GH_COMMIT_EMAIL`), pushes, and
   `gh pr create` (an existing PR for the branch is reported instead of failing).

Flags: `-d/--device`, `-b/--base`, `--branch`, `--exclude a,b,c`, `--skip-existing`,
`--new-campaign`, `--pull-images`, `--fail-fast`, `--no-pr`, `--dry-run`, `--install-deps`,
positional test names. Note `--update-args` on the providers word-splits, so pass
comma-separated values in a single word (`--exclude nim,zig`), never quoted lists.

---

## 9. Provider (`ovhcloud_bench.sh`)

One command to create a VM, set it up and start the benchmark in a tmux session named
`bench`, logging to `/opt/related-post-gen-bench/bench.log` and writing
`bench.done` (`status:`, `pull request:`, `finished:`) when the run ends.

- Commands: `run` (default), `attach`, `status`, `logs`, `wait`, `list`, `destroy`.
- Local state: `~/.cache/related-post-gen/ovhcloud-bench.env` (instance id, address,
  ssh user, device label). Re-running reuses a tracked instance.
- The pane runs `bash -lc '<run-bench.sh>; exec bash'`, so **attach after a run gives you a
  shell**, and "already running" is detected by *session exists and no `bench.done`*.
- Setup rewrites the readme's `##### VM Specs` line to `--device` (disable with
  `--no-label-sync`); a manual `./update_results.sh` run does **not** touch it.
- Pass bench options through `--update-args`; it is **word-split**, so comma-separate:
  `--update-args "--pull-images --tests go,go_ho"`.
- `destroy` only clears the local state once the instance has actually disappeared; an
  async delete that sticks leaves an instance holding quota (and billing).
- A create that fails or is interrupted still gets tracked by name, and a run that finds an
  untracked instance under the same `--name` adopts it — so a leftover cannot silently eat
  the project's RAM quota.

OVHcloud specifics:

- Public Cloud **Metal Instances** (`bm-s1`/`bm-m1`/`bm-l1`). Needs a Public Cloud project
  (`--project`, else `OVH_CLOUD_PROJECT_SERVICE`, else the only one in the account) and an
  `ovhcloud` token granting the `/cloud` routes.
- Flavor/region come from `cloud instance flavor list` and prefer a region with a real
  quota profile (`8vms`), not the discovery-mode `limited` one.
- Traps: instance creation is **v1** and resolves `--ssh-key.name` against the v1 `sshkey`
  store, which is separate from the v2 store the CLI's `cloud ssh-key` writes (hence
  `--ssh-key.create.*` with a name fallback); RAM/cores are counted **per project**, so an
  instance left behind makes the next create fail with `conflict: ram quota … reached`; a
  fresh server runs `unattended-upgrades`, which the setup disables before benchmarking so
  it neither holds the dpkg lock nor steals CPU from the timings; and the CLI's `--wait`
  can fail on a transient 5xx while the instance itself is fine.

---

## 10. Registry (`ghcr_images.sh`)

`push` / `pull` / `list` for the images, tagged
`ghcr.io/<owner>/<repo>/<image>:<sha256, 12 chars>`, where the hash covers the
generated Dockerfile **and `docker_start.sh`** (every image copies it in, so the
harness is image content). So **editing any Dockerfile or the harness invalidates
the published tag** — a pull misses and the image is rebuilt. The same hash is
computed by `ghcr_images.sh` and `.github/workflows/build-images.yml`; keep them
identical or pulls silently miss. Every image also carries a `:latest` alias, advanced only by builds on
`main`; it is for humans (`docker pull .../rust:latest`), never for the harness, so the
benchmark keeps pulling the content tag and cannot run an image that does not match its
Dockerfile. `ghcr_images.sh latest` repoints the alias from the registry without rebuilding
(plain manifest copy), which is also how to add it to already published images. Building all of them in CI: run the **Build images** workflow (`gh workflow run build-images.yml`);
it defaults to build-only, and `-f push=true` publishes. Tags are computed the same way, so
`ghcr_images.sh pull` finds them.

Pushing needs a token with `write:packages`
(`gh auth refresh -h github.com -s write:packages,read:packages`); pulling public packages
needs nothing. Keep packages public — private GHCR storage is metered.

---

## 11. Invariants and traps

- **One** `Processing time (w/o IO)` line per executed run; extra lines skew the average.
- **Never build or test an image on the local machine.** Verify in a Vercel Sandbox
  (`scripts/vercel-run.sh`, §5); local Docker/Podman only with explicit user approval.
- An image's tag covers `docker_start.sh` as well as its Dockerfile, so a harness
  edit republishes every image (see §10).
- Every language must produce **three** dataset sections or extraction bails.
- `$outfile` is absolute in `run.sh`; runners may `cd` deep.
- Benchmarks measure the **pushed** commit (containers clone `origin`); push before running.
- `results/raw_results_*.md` are **committed** — "file exists" cannot mean "already ran this
  campaign"; hence `results/.campaign-ignore`.
- The readme tables are generated. Never edit them by hand; run `extract_results.dart`.
  Never delete the HO blockquote (the extractor preserves it, but keep that code).
- `##### VM Specs ( … )` must describe the machine; the providers set it, manual runs don't.
- Timing hygiene: one test at a time, idle machine, no builds during a run. Numbers are
  **not** comparable across providers/machines — that is what the VM label is for.
- A newer toolchain can break sources for reasons unrelated to the code (Zig `std.fs.cwd`,
  nimble path handling, clang renames). Pin versions in the fragment.
- `docker_run.sh all` iterates a bash associative array (unordered) and runs each test with
  `sudo docker run`; `update_results.sh` has its own ordered loop. Only the latter is used
  for result updates.

---

## 12. Common tasks

```bash
# one language, locally, with verification
./run.sh go out.md && python3 verify.py related_posts_go.json

# generate an image's Dockerfile (no build)
./gen_dockerfile.sh go

# build + test one image: Vercel Sandbox, never local docker (see §5)
.agents/skills/update-toolchains/scripts/vercel-run.sh --worktree . \
  --dockerfile dockerfiles/go.Dockerfile --tag go_test go

# build/publish every image in CI (content-tag aware; skips what is published)
gh workflow run build-images.yml -f push=true

# a subset on a Metal Instance, into a new results branch + PR
./ovhcloud_bench.sh --project <id> --update-args "--tests go,go_ho,rust --skip-existing"

# resume a failed campaign (same branch = same PR)
./ovhcloud_bench.sh --update-args "--skip-existing --branch results-<timestamp>"

# regenerate the readme only
dart extract_results.dart results

# publish / restore images
./ghcr_images.sh push go,rust,zig      # or no names for all
./ghcr_images.sh list
```

---

## 13. Current state (as of the Debian migration)

- 34 self-contained images, 68 tests, no Arch packages anywhere. `inko` was dropped
  (never published, no results file, Arch-AUR-only recipe).
- Heavy images are `swift`, `dotnet`, `java` (GraalVM + sbt), `base` (Dart SDK + clang).
- Version pins live in the fragments with comments explaining why (zig, odin, nim, v, c3,
  neat, ldc, alire, hxcpp).
- The Debian migration has **not** been build-tested as a whole; first run will surface
  missing package names. Fix per language — each image is independent.
- `run.sh` carries two behavioural fixes worth preserving: the absolute results path (for
  fsharp's two-level `cd`) and `build_nim_related` (explicit `--path` for nim deps).
