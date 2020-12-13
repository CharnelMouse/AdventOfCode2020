x <- readLines("13.txt")
time <- as.integer(x[1])
ids <- unlist(strsplit(x[2], ","))
working <- ids != "x"
working_ids <- as.integer(ids[working])
waits <- -time %% working_ids
min_ind <- which.min(waits)
waits[min_ind] * working_ids[min_ind] # part one: 4938
# part two: solve x = -a mod b for all a,b, i.e. linear congruence problem
# Have to add package for large numbers here, base %% can't cope
# Only for as.bigz, mod.bigz, divq.bigz; not using gcd.bigz or gcdex
# i.e. only replacing base functions that break for big integers
library(gmp)
eeacc <- function(r1, r2, s1, s2, t1, t2) {
  if (r1 == 0)
    return(c(r2, s2, t2))
  q <- divq.bigz(r2, r1)
  eeacc(
    sub.bigz(r2, q*r1), r1,
    sub.bigz(s2, q*s1), s1,
    sub.bigz(t2, q*t1), t1
  )
}
extended_euclid <- function(val1, val2) eeacc(val2, val1, 0, 1, 1, 0)
resolve <- function(x, y) {
  val1 <- as.bigz(x, NA)
  val2 <- as.bigz(y, NA)
  mod1 <- modulus(x)
  mod2 <- modulus(y)
  if (mod1 == mod2) {
    if (val1 == val2)
      return(x)
    else
      stop(paste("no solutions for x =", val1, "%%", mod1, "and", val2, "%%", mod2))
  }
  eul_mod <- extended_euclid(mod1, mod2)
  as.bigz(
    divq.bigz(mod1*eul_mod[2]*val2 + mod2*eul_mod[3]*val1, eul_mod[1]),
    divq.bigz(mod1*mod2, eul_mod[1])
  )
}
set_times <- which(working) - 1L
# part two: 230903629977901 mod 2283338533368659
res <- Reduce(resolve, as.bigz(-set_times, working_ids))
format(as.numeric(res), scientific = FALSE)
