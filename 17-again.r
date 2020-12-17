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
  iterate(state, dim(state)[2], dim(state)[1], dim(state)[3], iter)
}
res <- run(start, 6)
sum(res) + sum(res[, , -1]) # part one: 336

# both z and w axes are now symmetric, but are also interchangeable.
# in theory, we can therefore reduce storage size by only using the values where
# 0 <= w <= z (from-zero indexing).
# if a = max(abs(c(z, w))) and b = min(abs(c(z, w))), then
# index(z, w) = a(a+1)/2 + b,
# and when summing over all elements we count each index with the following
# weights:
# 1, 4, 4, 4, 8, 4, 4, 8, 8, 4, 4, 8, 8, 8, 4, ...
index <- function(z, w) {
  a <- pmax(abs(z), abs(w))
  b <- pmin(abs(z), abs(w))
  a*(a + 1)/2 + b
}
xpos <- function(index) {
  floor((sqrt(8*index + 1) - 1)/2)
}
weight_seq <- function(len) {
  slen <- xpos(len)
  unlist(lapply(
    0:slen,
    function(s) {
      switch(1 + min(s, 2), 0, c(4, 4), c(4, rep(8, s - 1), 4))
    }
  ))[1:(len + 1)]
}
iterate2 <- function(state, ylen, xlen, zlen, iter) {
  if (iter == 0)
    return(state)
  outermost_z_indices <- (zlen - xpos(zlen - 1)):zlen
  expand_yl <- any(state[1, , ])
  expand_yr <- any(state[ylen, , ])
  expand_xl <- any(state[, 1, ])
  expand_xr <- any(state[, xlen, ])
  expand_z <- any(state[, , outermost_z_indices])
  new_ylen <- ylen + expand_yl + expand_yr
  new_xlen <- xlen + expand_xl + expand_xr
  new_zlen <- if (expand_z) zlen + xpos(zlen) + 1 else zlen
  new_lens <- c(new_ylen, new_xlen, new_zlen)
  # add empty cells at end of dims to prevent errors in neighbour lookups
  expanded_state <- array(FALSE, c(new_ylen + 1, new_xlen + 1, new_zlen))
  expanded_state[expand_yl + 1:ylen, expand_xl + 1:xlen, 1:zlen] <- state
  new_state <- array(FALSE, new_lens)
  for (x in 1:new_xlen) {
    for (y in 1:new_ylen) {
      for (z in 0:(new_zlen - 1)) {
        z_xpos <- xpos(z)
        z_pos <- c(z_xpos, z - z_xpos*(z_xpos + 1)/2)
        z_neighbours_pos <- outer(z_pos[1] + (-1):1, z_pos[2] + (-1):1, index)
        # if (any(z_neighbours_pos + 1 > new_zlen))
        #   stop(paste0(
        #     "too big!",
        #     "\nz (0-ind): ", paste(z, collapse = ", "),
        #     "\nneighbours (0-ind): ", paste(z_neighbours_pos, collapse = ", "),
        #     "\nz length: ", zlen,
        #     "\nnew z length: ", new_zlen,
        #     "\nexpanded state dims: ", paste(dim(expanded_state), collapse = ", "),
        #     "\nz dim expanded?: ", expand_z
        #   ))
        neighbours <- expanded_state[
          y + (-1):1,
          x + (-1):1,
          z_neighbours_pos[z_neighbours_pos < new_zlen] + 1
          ]
        new_state[y, x, z + 1] <- is.element(
          sum(neighbours),
          c(3, if (expanded_state[y, x, z + 1]) 4)
        )
      }
    }
  }
  iterate2(new_state, new_ylen, new_xlen, new_zlen, iter - 1)
}
run2 <- function(state, iter) {
  iterate2(state, dim(state)[2], dim(state)[1], dim(state)[3], iter)
}
res2 <- run2(start, 6)
weights2 <- rep(weight_seq(dim(res2)[3] - 1), each = prod(dim(res2)[-3]))
sum(res2*weights2) # part two: 2620
