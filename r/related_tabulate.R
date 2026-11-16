rlang::check_installed(c("yyjsonr", "here"))

TopN <- 5L
jsonText <- yyjsonr::read_json_file(here::here("posts.json"))
if (is.data.frame(jsonText)) {
  post_records <- lapply(seq_len(nrow(jsonText)), function(i) list(
    "_id" = jsonText[["_id"]][[i]],
    "title" = jsonText[["title"]][[i]],
    "tags" = jsonText[["tags"]][[i]]
  ))
} else {
  stop("Expected a JSON array of posts.")
}
post_tags <- lapply(post_records, function(post) {
  unique(as.character(unlist(post[["tags"]], use.names = FALSE)))
})
post_records <- lapply(seq_along(post_records), function(i) {
  post <- post_records[[i]]
  post[["tags"]] <- post_tags[[i]]
  post
})
posts_length <- length(post_tags)

start_time <- as.numeric(Sys.time())
postings <- split(
  rep.int(seq_len(posts_length), lengths(post_tags)),
  unlist(post_tags, use.names = FALSE)
)
top <- matrix(NA_integer_, nrow = posts_length, ncol = TopN)

for (i in seq_len(posts_length)) {
  hits <- unlist(postings[post_tags[[i]]], use.names = FALSE)
  counts <- tabulate(hits, nbins = posts_length)
  counts[i] <- -1L
  max_score <- length(post_tags[[i]])
  score_hist <- tabulate(counts + 1L, nbins = max_score + 1L)
  needed <- TopN
  cutoff <- 0L
  for (score in seq.int(max_score, 0L)) {
    if (score_hist[score + 1L] >= needed) {
      cutoff <- score
      break
    }
    needed <- needed - score_hist[score + 1L]
  }

  candidates <- which(counts >= cutoff)
  candidate_scores <- counts[candidates]
  higher <- candidates[candidate_scores > cutoff]
  higher <- higher[order(-counts[higher], higher, method = "radix")]
  top[i, seq_len(length(higher))] <- higher

  if (needed > 0L) {
    tied <- candidates[candidate_scores == cutoff]
    top[i, seq.int(length(higher) + 1L, TopN)] <- tied[seq_len(needed)]
  }
}

all_related_posts <- lapply(seq_len(posts_length), function(i) list(
  "_id" = post_records[[i]]$`_id`,
  "tags" = post_tags[[i]],
  "related" = post_records[top[i, ]]
))

end_time <- as.numeric(Sys.time())
cat("Processing time (w/o IO):", round(end_time - start_time, 4), "s\n")
yyjsonr::write_json_file(all_related_posts, here::here("related_posts_r.json"), pretty = TRUE, auto_unbox = TRUE)
