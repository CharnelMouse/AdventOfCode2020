x <- sort.int(as.integer(readLines("10.txt")))
diffs <- diff(c(0, x, max(x) + 3))
sum(diffs == 1)*sum(diffs == 3) # part one: 2812
facc <- function(queue, previous_sums, predecessors) {
  # tried using diffs instead of values, but it was slower
  if (length(queue) == 0)
    return(previous_sums[1])
  val <- queue[1]
  use <- predecessors - val <= 3
  new_sum <- sum(previous_sums[use])
  facc(
    queue[-1],
    c(new_sum, previous_sums[use[-3]]),
    c(val, predecessors[use[-3]])
  )
}
y <- rev(x)
res <- facc(c(y[-1], 0), 1, y[1])
format(res, scientific = FALSE) # part two: 386869246296064
