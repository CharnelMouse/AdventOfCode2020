x <- paste(readLines("6.txt"), collapse = "\n")
groups <- strsplit(
  unlist(strsplit(x, "\n\n", fixed = TRUE)),
  "\n",
  fixed = TRUE
)
union_lengths <- vapply(
  groups,
  function(x) length(unique(unlist(strsplit(x, "", fixed = TRUE)))),
  integer(1)
)
sum(union_lengths) # part one 6587
intersection_lengths <- vapply(
  groups,
  function(x) length(Reduce(intersect, strsplit(x, "", fixed = TRUE))),
  integer(1)
)
sum(intersection_lengths) # part two 3235
