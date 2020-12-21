x <- readLines("07.txt")
cleaned <- Reduce(
  function(str, ch) gsub(ch, ":", str, fixed = TRUE),
  c(", ", " contain "),
  init = Reduce(
    function(str, rem) gsub(rem, "", str, fixed = TRUE),
    c(" bags", " bag", "."),
    init = x
  ),
)
parsed <- lapply(
  strsplit(cleaned, ":", fixed = TRUE),
  function(strs) if (identical(strs[2], "no other")) strs[1] else strs
)
from <- vapply(parsed, `[`, character(1), 1)
children <- function(parsed_el) {
  ns <- as.integer(substr(parsed_el[-1], 1, 1)) # assumes no 10+ entries
  nms <- substring(parsed_el[-1], 3)
  indices <- match(from, nms)
  ifelse(is.na(indices), 0L, ns[indices])
}
contained <- mapply(children, parsed)
can_hold <- function(one_of) {
  nxt <- one_of | (one_of %*% contained)
  if (all(nxt == one_of)) one_of else can_hold(nxt)
}
n_bags <- function(outermost) {
  s <- sum(outermost)
  if (s == 0) 0L else s + n_bags(contained %*% outermost)
}
start <- from == "shiny gold"
sum(can_hold(start)) - 1 # part one: 300
n_bags(start) - 1 # part two: 8030
# part two alternative solution, not used just in case of floating-point problems:
# sum(round(solve(diag(length(from)) - contained, start))) - 1
