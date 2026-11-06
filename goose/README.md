# Goose

Implementation notes for the Goose entry (`related.goose`). It mirrors the Go
reference: parse `../posts.json`, build a tag → posts index, accumulate shared
tags per post, keep the top 5 and write `../related_posts_goose.json`. Run it
with `./run.sh goose` (or `goose build -o related related.goose && ./related`
from this directory).

## Why shared-tag counting is ~2× slower than Go

In the 60k-post profile the two implementations track each other on the counter
reset and on the top-5 scan; essentially the whole gap is the shared-tag
counting loop. The cause is a redundant bounds check, not codegen quality.

The hotspot is the CSR walk in `top5`:

```goose
for k in p.tags_len {
    let tag = post_tags[p.tags_off + k] as i64;
    let s = tag_start[tag];
    let e = tag_start[tag + 1];
    for c in s..e { counts[by_tag[c] as i64] += 1; }
}
```

Goose emits **two** bounds checks per element — one for `by_tag[c]`, one for
`counts[...]`:

```c
/* generated C, gcc -O2 */
.L1100:
    cmpq  %r13, %rdi            /* c vs by_tag.len  -> bounds check #1 */
    jnb   .L1229
    movl  (%rbx,%rdi,4), %eax   /* v = by_tag[c] */
    cmpq  %r14, %rax            /* v vs counts.len  -> bounds check #2 */
    jnb   .L1230
    addq  $1, %rdi
    addb  $1, (%r12,%rax)       /* counts[v]++ */
    cmpq  %rdi, %rdx
    jne   .L1100
```

Go's equivalent loop, ranging over the tag's slice, only needs the check on the
counter array:

```asm
    MOVL  0(DX)(R8*4), R11      ; v = slice[i]
    CMPQ  R9, R11
    JA    body                  ; single bounds check
    MOVZX 0(R11)(R10*1), R12
    INCL  R12
    MOVB  R12, 0(R10)(R11*1)
```

The compiler cannot prove `c ∈ [tag_start[tag], tag_start[tag+1])` is within
`by_tag.len`, so `GS_IDX` inserts a check on every one of the ~2.6 billion
inner iterations. Over that many iterations the extra dependent load, compare
and branch roughly doubles the loop. (Goose's increment is in fact cheaper — a
single `addb` versus Go's `MOVZX`/`INCL`/`MOVB` — so the check is the entire
gap.)

### Measurement

Isolated counting-only run, 60,000 posts, same dataset, no per-post timers
(OVHcloud bm-s1, three runs averaged; identical checksum `7642165`):

| Build                                   | Shared-tag counting |
| --------------------------------------- | ------------------: |
| Goose (`related.goose`)                 |           2,161 ms  |
| Go reference                            |           1,130 ms  |
| Goose, CSR segment walked as a slice    |           ~1,112 ms |

Walking the segment as a slice,

```goose
for v in by_tag[s..e] { counts[v as i64] += 1; }
```

hoists the `by_tag` check out of the loop (only the `counts` check remains) and
brings Goose to parity with Go. The bound it relies on is real, so this is not a
disabled check.

The published benchmark runs on the unmodified source; that one-line change
would lift the Goose result but must go through a fresh benchmark campaign, so
it is deliberately not applied here. The full phase breakdown is in
[zupat/related_post_gen#599](https://github.com/zupat/related_post_gen/pull/599).
