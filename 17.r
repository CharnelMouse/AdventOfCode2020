x <- do.call(rbind, strsplit(readLines("17.txt"), "", fixed = TRUE)) == "#"
# starting state and rules are z-symmetric, so needn't track strictly negative z
iterate <- function(state, ylen, xlen, zlen, iter) {
  if (iter == 0)
    return(state)
  expand_yl <- any(state[1, , ])
  expand_yr <- any(state[ylen, , ])
  expand_xl <- any(state[, 1, ])
  expand_xr <- any(state[, xlen, ])
  expand_z <- any(state[, , zlen])
  new_ylen <- ylen + expand_yl + expand_yr
  new_xlen <- xlen + expand_xl + expand_xr
  new_zlen <- zlen + expand_z
  new_lens <- c(new_ylen, new_xlen, new_zlen)
  # add empty cells at end of dims to prevent errors in neighbour lookups
  expanded_state <- array(
    FALSE,
    dim = new_lens + 1
  )
  expanded_state[expand_yl + 1:ylen, expand_xl + 1:xlen, 1:zlen] <- state
  new_state <- array(FALSE, new_lens)
  for (y in 1:new_ylen) {
    for (x in 1:new_xlen) {
      for (z in 0:(new_zlen - 1)) {
        neighbours <- sum(expanded_state[y + (-1):1, x + (-1):1, abs(z + (-1):1) + 1])
        new_state[y, x, z + 1] <- is.element(
          neighbours,
          c(3, if (expanded_state[y, x, z + 1]) 4)
        )
      }
    }
  }
  iterate(new_state, new_ylen, new_xlen, new_zlen, iter - 1)
}
run <- function(state, iter) {
  iterate(array(state, c(dim(state), 1)), dim(state)[2], dim(state)[1], 1, iter)
}
res <- run(x, 6)
sum(res) + sum(res[, , -1]) # part one: 336

# Both z and w axes are now symmetric, but are also exchangeable,
# so we only store values in the (z, w) segment where 0 <= w <= z.
# Using zero-based indexing, since it's a bit easier:
#       9
#     5 8
#   2 4 7
# 0 1 3 6 <- triangular numbers on bottom (one-based puts them on top)
# When summing over cells at the end, we weight the recorded cells with the
# following weights:
# 1, 4, 4, 4, 8, 4, 4, 8, 8, 4, 4, 8, 8, 8, 4, ...
index <- function(z, w) {
  az <- abs(z)
  aw <- abs(w)
  ifelse(az < aw, aw*(aw + 1)/2 + az, az*(az + 1)/2 + aw)
}
ind_zval <- function(index) {
  floor((sqrt(8*index + 1) - 1)/2)
}
weight_seq <- function(len) {
  slen <- ind_zval(len)
  unlist(lapply(
    0:slen,
    function(s) {
      switch(1 + min(s, 2), 0, c(4, 4), c(4, rep(8, s - 1), 4))
    }
  ))[1:(len + 1)]
}
iterate2 <- function(state, ylen, xlen, ilen, ilen_zval, iter) {
  if (iter == 0)
    return(state)
  outermost_i_indices <- (ilen + 1 - ilen_zval):ilen
  expand_yl <- any(state[1, , ])
  expand_yr <- any(state[ylen, , ])
  expand_xl <- any(state[, 1, ])
  expand_xr <- any(state[, xlen, ])
  expand_z <- any(state[, , outermost_i_indices])
  new_ylen <- ylen + expand_yl + expand_yr
  new_xlen <- xlen + expand_xl + expand_xr
  new_ilen <- if (expand_z) ilen + ilen_zval + 1 else ilen
  new_lens <- c(new_ylen, new_xlen, new_ilen)
  # add empty cells at end of dims to prevent errors in neighbour lookups
  expanded_state <- array(FALSE, c(new_ylen + 1, new_xlen + 1, new_ilen))
  expanded_state[expand_yl + 1:ylen, expand_xl + 1:xlen, 1:ilen] <- state
  new_state <- array(FALSE, new_lens)
  for (y in 1:new_ylen) {
    for (x in 1:new_xlen) {
      for (i in 0:(new_ilen - 1)) {
        i_zval <- ind_zval(i)
        i_zw <- c(i_zval, i - i_zval*(i_zval + 1)/2)
        i_neighbour_ivals <- index(
          rep(i_zw[1] + (-1):1, 3),
          rep(i_zw[2] + (-1):1, each = 3)
        )
        neighbours <- expanded_state[
          y + (-1):1,
          x + (-1):1,
          i_neighbour_ivals[i_neighbour_ivals < new_ilen] + 1
          ]
        new_state[y, x, i + 1] <- is.element(
          sum(neighbours),
          c(3, if (expanded_state[y, x, i + 1]) 4)
        )
      }
    }
  }
  iterate2(new_state, new_ylen, new_xlen, new_ilen, ind_zval(new_ilen), iter - 1)
}
run2 <- function(state, iter) {
  iterate2(array(state, c(dim(state), 1)), dim(state)[2], dim(state)[1], 1, 1, iter)
}
res2 <- run2(x, 6)
weights2 <- rep(weight_seq(dim(res2)[3] - 1), each = prod(dim(res2)[-3]))
sum(res2*weights2) # part two: 2620
