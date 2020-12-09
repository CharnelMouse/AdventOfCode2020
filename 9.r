# The brief version of this takes so long that I just give the quick version.
x <- as.numeric(readLines("9.txt")) # later numbers too large for R ints
second <- function(n) cumsum(c(n - 1, if (n > 2) 23:(26 - n)))
by_second <- unlist(lapply(2:25, second)) # Sum order by second index first
rem_ind <- cumsum(c(1, 1:23)) # Sums involving first precedent
find_nonsum <- function(remaining, precedents, sums) {
  if (length(remaining) == 0)
    return(NA_real_)
  n <- remaining[1]
  if (!is.element(n, sums))
    n
  else
    find_nonsum(
      remaining[-1],
      c(precedents[-1], n),
      c(sums[-rem_ind], precedents[-1] + n)
    )
}
start <- x[1:25]
# Arrange sums in order of second index, since this is order in which added later
sums_start <- colSums(combn(start, 2))[by_second]
n <- find_nonsum(x[-(1:25)], start, sums_start)
n # part one: 41682220
worm <- function(total, vec, start, end, s) {
  if (s == total)
    return(vec[start:end])
  if (s < total)
    worm(total, vec, start, end + 1L, s + vec[end + 1L])
  else
    worm(total, vec, start + 1L, end, s - vec[start])
}
find <- function(total, vec) worm(total, vec, 1L, 1L, vec[1])
vec <- find(n, x)
min(vec) + max(vec) # part two: 5388976
