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
eeacc <- function(r, s, t) {
  if (r[1] == 0)
    return(c(gcd = r[2], bezout1 = s[2], bezout2 = t[2]))
  new_r <- mod.bigz(r[2], r[1])
  if (new_r < 0)
    stop(paste("negative new_r:", r[2], "%%", r[1], "=", new_r))
  q <- divq.bigz(r[2], r[1])
  new_s <- s[2] - q*s[1]
  new_t <- t[2] - q*t[1]
  eeacc(c(new_r, r[1]), c(new_s, s[1]), c(new_t, t[1]))
}
extended_euclid <- function(val1, val2) {
  if (val1 < val2)
    stop(paste("val1 must be larger", val1, val2))
  if (val2 < 0)
    stop("negative val2")
  r <- c(val2, val1)
  s <- c(0, 1)
  t <- c(1, 0)
  eeacc(r, s, t)
}
modinv <- function(val, mod) {
  val <- mod.bigz(val, mod)
  euc <- extended_euclid(mod, val)
  if (euc[1] != 1)
    stop("GCD > 1, no inverse")
  mod.bigz(euc[3], mod)
}
congsolve <- function(val1, mod1, val2, mod2) {
  new_mod <- mod1*mod2
  left <- mod.bigz((mod2 - mod1), new_mod)
  right <- mod.bigz((val1*mod2 - val2*mod1), new_mod)
  left_gcd <- extended_euclid(new_mod, left)[1]
  if (left_gcd != 1) {
    left <- left/left_gcd
    right <- right/left_gcd
    new_mod <- new_mod/left_gcd
  }
  leftinv <- modinv(left, new_mod)
  sol <- mod.bigz((leftinv*right), new_mod)
  c(sol, new_mod)
}
res <- Reduce(
  function(x, y) {
    congsolve(x[1], x[2], y[1], y[2])
  },
  lapply(
    seq_along(set_times)[-1],
    function(n) c(as.bigz(-set_times[n] %% working_ids[n]), as.bigz(working_ids[n]))
  ),
  init = c(-as.bigz(set_times[1]), as.bigz(working_ids[1]))
)
res[1] # part two: 230903629977901
