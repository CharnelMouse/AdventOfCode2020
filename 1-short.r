x <- as.integer(readLines("01.txt"))
fprod <- function(x, n, s) {
  combns <- combn(x, n)
  prod(combns[, colSums(combns) == s])
}
fprod(x, 2, 2020) # part one: 776064
fprod(x, 3, 2020) # part two: 6964400
