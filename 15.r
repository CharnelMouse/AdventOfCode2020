# original version didn't assign the last_app vector in advance
# instead, it had a second, growing, vector tracking numbers seen so far,
# so each iteration matched current number in that vector first,
# then used that as index for last_app vector.
# This would have taken a very long time to run in part two,
# so assigning a large last_app vector in advance is better.
numbers <- as.numeric(unlist(strsplit(readLines("15.txt"), ",")))
# numbers <- as.integer(c(0,5,4,1,10,14,7))
iterate <- function(current_n, current_index, end, last_apps) {
  # use while loops instead of recursion, else part two runs into stack limit
  # probably faster with a hash, but I'm trying to do these in base R
  while (current_index < end) {
    current_last_app <- last_apps[current_n + 1]
    if (!anyNA(current_last_app)) { # marginally faster than !is.na(current_last_app)
      last_apps[current_n + 1] <- current_index
      current_n <- current_index - current_last_app
      current_index <- current_index + 1
    }else{
      last_apps[current_n + 1] <- current_index
      current_n <- 0
      current_index <- current_index + 1
    }
  }
  current_n
}
start <- function(numbers, end) {
  last_apps <- rep(NA_real_, end)
  n_start <- length(numbers)
  # works even if start contains duplicates
  # hold back last number to start iterator without adding manual next value
  last_apps[numbers[-n_start] + 1] <- seq.int(n_start - 1)
  if (n_start > end)
    stop("end must be no larger than numbers length")
  iterate(numbers[n_start], n_start, end, last_apps)
}
start(numbers, 2020) # part one: 203
start(numbers, 30000000) # part two: 9007186
