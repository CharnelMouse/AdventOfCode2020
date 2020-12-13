x <- readLines("13.txt")
time <- as.integer(x[1])
ids <- unlist(strsplit(x[2], ","))
working <- ids != "x"
working_ids <- as.integer(ids[working])
waits <- -time %% working_ids
min(waits) * working_ids[which.min(waits)] # part one: 4938
# part two: solve x = -a mod b for all a,b, i.e. linear congruence problem
# ax = c mod f and bx = d mod g has solution
# ax - mf = c, bx - ng = d for some m, n
# agx - mfg = cg, bfx - nfg = df,
# agx = cg mod fg, bfx = df mod fg,
# (ag-bf)x = cg-df mod bd
# x = (ag-bf)^{-1} (cg-df) mod bd
set_times <- which(working) - 1L
# Have to add package for large numbers here, base %% can't cope
# Only for as.bigz, mod.bigz, divq.bigz; not using gcd.bigz, inv.bigz
# i.e. only replacing base functions that break for big integers
library(gmp)
# we make functions for modular inverse first, using extended Euler
# r = gcd, s and t are s.t. s*x + t*y = 1
# we don't care about s, since x is the modulus and will disappear
# when calculating inverse, so we don't track it
gcd <- function(x, y) {
  if (x == 0)
    return(y)
  gcd(mod.bigz(y, x), x)
}
eeacc <- function(r1, r2, t1, t2) {
  if (r1 == 0)
    return(c(r2, t2))
  q <- divq.bigz(r2, r1)
  eeacc(sub.bigz(r2, q*r1), r1, sub.bigz(t2, q*t1), t1)
}
extended_euclid <- function(val1, val2) {
  if (val1 < val2)
    stop(paste("val1 must be larger", val1, val2))
  if (val2 < 0)
    stop("negative val2")
  eeacc(val2, val1, 1, 0)
}
modinv <- function(val, mod) {
  val <- mod.bigz(val, mod)
  euc <- extended_euclid(mod, val)
  if (euc[1] != 1)
    stop(paste0("gcd(", val, ", ", mod, ") = ", euc[1], " > 1, no inverse"))
  as.bigz(euc[2], mod)
}
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
  mod_gcd <- gcd(min(mod1, mod2), max(mod1, mod2))
  left <- divq.bigz(mod2 - mod1, mod_gcd)
  right <- divq.bigz(val1*mod2 - val2*mod1, mod_gcd)
  new_mod <- divq.bigz(mod1*mod2, mod_gcd)
  modinv(left, new_mod)*right
}
congruences <- as.bigz(-set_times, working_ids)
res <- Reduce(resolve, congruences)
format(as.numeric(res), scientific = FALSE) # part two: 230903629977901
