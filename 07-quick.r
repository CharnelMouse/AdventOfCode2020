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
  substring(parsed_el[-1], 3)
}
can_hold <- function(queue, visiting, visited) {
  is_direct_holder <- vapply(
    parsed[match(queue, from)],
    function(x) any(is.element(visiting, children(x))),
    logical(1)
  )
  direct_holders <- queue[is_direct_holder]
  if (length(direct_holders) == 0)
    c(visited, visiting)
  else
    can_hold(queue[!is_direct_holder], direct_holders, c(visited, visiting))
}
children_counts <- function(parsed_el) {
  ns <- as.integer(substr(parsed_el[-1], 1, 1)) # assumes no 10+ entries
  nms <- substring(parsed_el[-1], 3)
  setNames(ns, nms)
}
n_within <- function(holders, n) {
  counts <- Reduce(
    c,
    Map(
      function(parsed_el, n) {
        n*children_counts(parsed_el)
      },
      parsed[match(holders, from)],
      n
    )
  )
  s <- sum(counts)
  if (s == 0)
    0L
  else
    s + n_within(names(counts), unname(counts))
}
# part one: 300
length(can_hold(setdiff(from, "shiny gold"), "shiny gold", character())) - 1L
# part two: 8030
n_within("shiny gold", 1L)
