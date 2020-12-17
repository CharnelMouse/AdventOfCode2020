x <- do.call(rbind, strsplit(readLines("17.txt"), "", fixed = TRUE)) == "#"
start <- array(x, dim = c(dim(x), 1))
chars <- function(x) ifelse(x, "#", ".")
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
  for (x in 1:new_xlen) {
    for (y in 1:new_ylen) {
      for (z in 1:new_zlen) {
        neighbours <- expanded_state[y + (-1):1, x + (-1):1, z + (-1):1]
        # neighbours lengths varies depending on whether on left-hand boundary,
        # since e.g. x[0:2] returns an two-length value
        xy_len <- 9 - 3*sum(c(x, y) == 1) + all(c(x, y) == 1)
        weights <- rep(
          c(1, 1 + (z == 1)),
          c(xy_len*(2 - (z == 1)), xy_len)
        )
        weighted_sum <- sum(neighbours*weights)
        new_state[y, x, z] <- is.element(
          weighted_sum,
          c(3, if (expanded_state[y, x, z]) 4)
        )
      }
    }
  }
  iterate(new_state, new_ylen, new_xlen, new_zlen, iter - 1)
}
run <- function(state, iter) {
  iterate(state, dim(state)[2], dim(state)[1], dim(state)[3], iter)
}
res <- run(start, 6)
sum(res) + sum(res[, , -1]) # part one: 336
# state is now (z,w)-symmetric
# also (z,w)-exchangeable, not sure how to use this yet
start2 <- array(x, dim = c(dim(x), 1, 1))
iterate2 <- function(state, ylen, xlen, zlen, wlen, iter) {
  if (iter == 0)
    return(state)
  expand_yl <- any(state[1, , , ])
  expand_yr <- any(state[ylen, , , ])
  expand_xl <- any(state[, 1, , ])
  expand_xr <- any(state[, xlen, , ])
  expand_z <- any(state[, , zlen, ])
  expand_w <- any(state[, , , wlen])
  new_ylen <- ylen + expand_yl + expand_yr
  new_xlen <- xlen + expand_xl + expand_xr
  new_zlen <- zlen + expand_z
  new_wlen <- wlen + expand_w
  new_lens <- c(new_ylen, new_xlen, new_zlen, new_wlen)
  # add empty cells at end of dims to prevent errors in neighbour lookups
  expanded_state <- array(FALSE, new_lens + 1)
  expanded_state[expand_yl + 1:ylen, expand_xl + 1:xlen, 1:zlen, 1:wlen] <- state
  new_state <- array(FALSE, new_lens)
  for (x in 1:new_xlen) {
    for (y in 1:new_ylen) {
      for (z in 1:new_zlen) {
        for (w in 1:new_wlen) {
          neighbours2 <- expanded_state[y + (-1):1, x + (-1):1, z + (-1):1, w + (-1):1]
          # neighbours lengths varies depending on whether on left-hand boundary,
          # since e.g. x[0:2] returns an two-length value
          xy_len <- 9 - 3*sum(c(x, y) == 1) + all(c(x, y) == 1)
          xyz_weights <- rep(
            c(1, 1 + (z == 1)),
            c(xy_len*(2 - (z == 1)), xy_len)
          )
          xyz_len <- length(xyz_weights)
          weights <- rep(xyz_weights, 3 - (w == 1)) *
            rep(
              c(1, 1 + (w == 1)),
              c(xyz_len*(2 - (w == 1)), xyz_len)
            )
          weighted_sum <- sum(neighbours2*weights, na.rm = TRUE)
          new_state[y, x, z, w] <- is.element(
            weighted_sum,
            c(3, if (expanded_state[y, x, z, w]) 4)
          )
        }
      }
    }
  }
  iterate2(new_state, new_ylen, new_xlen, new_zlen, new_wlen, iter - 1)
}
run2 <- function(state, iter) {
  iterate2(state, dim(state)[2], dim(state)[1], dim(state)[3], dim(state)[4], iter)
}
res2 <- run2(start2, 6)
sum(res2) + sum(res2[, , -1, ]) + sum(res2[, , , -1]) + sum(res2[, , -1, -1]) # part two: 2620
