x <- as.integer(readLines("1.txt"))

# Part one: product of two numbers that sum to 2020

combns <- combn(x, 2)
combnsums <- colSums(combns)
stopifnot(sum(combnsums == 2020) == 1)
pair <- combns[, combnsums == 2020]
prod(pair)

# Part two: product of three numbers etc.

combns2 <- combn(x, 3)
combnsums2 <- colSums(combns2)
stopifnot(sum(combnsums2 == 2020) == 1)
pair2 <- combns2[, combnsums2 == 2020]
prod(pair2)

# Neat version

x <- as.integer(readLines("1.txt"))
pairprod <- function(
  x,
  n,
  s
) {
  combns <- combn(x, n)
  sum_is_s <- colSums(combns) == s
  stopifnot(sum(sum_is_s) == 1)
  set <- combns[, sum_is_s]
  prod(set)
}
stopifnot(pairprod(x, 2, 2020) == prod(pair))
stopifnot(pairprod(x, 3, 2020) == prod(pair2))

# Really short version

fprod <- function(
  x,
  n,
  s
) {
  combns <- combn(x, n)
  prod(combns[, colSums(combns) == s])
}
stopifnot(fprod(x, 2, 2020) == prod(pair))
stopifnot(fprod(x, 3, 2020) == prod(pair2))
