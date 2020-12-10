x <- sort(as.integer(readLines("10.txt")))
prod(table(diff(c(0, x, max(x) + 3)))[c("1", "3")]) # part one: 2812
sum_successors <- function(value, values, sums) {
  sum(sums[values - value <= 3])
}
acc <- function(values, current_sums, index) {
  if (index > length(values))
    return(current_sums[index - 1])
  val <- values[index]
  poss_inds <- (index - 3:1)[index - 3:1 >= 1]
  new_sum <- sum_successors(val, values[poss_inds], current_sums[poss_inds])
  acc(values, c(current_sums, new_sum), index + 1L)
}
res <- acc(c(rev(x), 0), 1L, 2L)
format(res, scientific = FALSE) # part two: 386869246296064
