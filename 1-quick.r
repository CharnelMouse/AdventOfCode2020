x <- as.integer(readLines("1.txt"))
fprod <- function(x, n, s) {
  if (n <= 0 || length(x) == 0)
    return(NA_integer_)
  if (n == 1) {
    if (any(x == s))
      as.integer(s)
    else
      NA_integer_
  }else{
    first <- x[1] * fprod(x[-1], n - 1, s - x[1])
    if (!is.na(first))
      first
    else
      fprod(x[-1], n, s)
  }
}
sorted <- sort(x)
fprod(sorted, 2, 2020) # part one: 776064
fprod(sorted, 3, 2020) # part two: 6964400
