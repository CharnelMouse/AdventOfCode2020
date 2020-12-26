subject_number <- 7
mod <- 20201227
keys <- strtoi(readLines("25.txt"))
# subject_number^x %% mod = keys,
# so part one answer is key1^x2 %% mod = key2^x1 %% mod
# we only need one power, x1
find_power <- function(start, end, mod) {
  pow <- 0
  val <- 1
  while (val != end) {
    val <- (val*start) %% mod
    pow <- pow + 1
  }
  pow
}
pow1 <- find_power(subject_number, keys[1], mod)
apply_power <- function(start, n, mod) {
  val <- 1
  while (n > 0) {
    val <- (val*start) %% mod
    n <- n - 1
  }
  val
}
apply_power(keys[2], pow1, mod) # part one: 3015200
