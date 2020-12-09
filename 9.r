x <- as.numeric(readLines("9.txt")) # later numbers too large for R ints
find_nonsum <- function(remaining, visited, sums) {
  # forming the sums with combn for each iteration is
  # briefer, but takes such a long time that I don't
  # think it's worth splitting into brief/quick scripts for
  if (length(remaining) == 0)
    return(NA_real_)
  n <- remaining[1]
  if (!is.element(n, sums))
    remaining[1]
  else
    find_nonsum(remaining[-1], c(visited, n), c(sums, visited + n))
}
start <- x[1:25]
sums <- colSums(combn(start, 2))
n <- find_nonsum(x[-(1:25)], start, sums)
n # part one: 41682220
worm <- function(total, vec, start, end, s) {
  if (s == total)
    return(vec[start:end])
  if (s < total)
    worm(total, vec, start, end + 1L, s + vec[end + 1L])
  else
    worm(total, vec, start + 1L, end, s - vec[start])
}
find <- function(total, vec) {
  worm(total, vec, 1L, 1L, vec[1])
}
vec <- find(n, x)
min(vec) + max(vec) # part two: 5388976
