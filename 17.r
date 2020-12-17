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
  # add empty cells at end of dims to prevent errors in neighbour lookups
  expanded_state <- array(
    FALSE,
    dim = c(new_ylen + 1, new_xlen + 1, new_zlen + 1)
  )
  expanded_state[expand_yl + 1:ylen, expand_xl + 1:xlen, 1:zlen] <- state
  new_state <- array(
    FALSE,
    dim = c(new_ylen, new_xlen, new_zlen)
  )
  new_state[expand_yl + 1:ylen, expand_xl + 1:xlen, 1:zlen] <- state
  new_state <- array(FALSE, c(new_ylen, new_xlen, new_zlen))
  for (x in 1:new_xlen) {
    for (y in 1:new_ylen) {
      for (z in 1:new_zlen) {
        neighbours <- sum(
          expanded_state[y - 1, x - 1, z - 1],
          expanded_state[y    , x - 1, z - 1],
          expanded_state[y + 1, x - 1, z - 1],
          expanded_state[y - 1, x    , z - 1],
          expanded_state[y    , x    , z - 1],
          expanded_state[y + 1, x    , z - 1],
          expanded_state[y - 1, x + 1, z - 1],
          expanded_state[y    , x + 1, z - 1],
          expanded_state[y + 1, x + 1, z - 1],
          expanded_state[y - 1, x - 1, z    ],
          expanded_state[y    , x - 1, z    ],
          expanded_state[y + 1, x - 1, z    ],
          expanded_state[y - 1, x    , z    ],
          expanded_state[y + 1, x    , z    ],
          expanded_state[y - 1, x + 1, z    ],
          expanded_state[y    , x + 1, z    ],
          expanded_state[y + 1, x + 1, z    ],
          expanded_state[y - 1, x - 1, z + 1] * (1 + (z == 1)),
          expanded_state[y    , x - 1, z + 1] * (1 + (z == 1)),
          expanded_state[y + 1, x - 1, z + 1] * (1 + (z == 1)),
          expanded_state[y - 1, x    , z + 1] * (1 + (z == 1)),
          expanded_state[y    , x    , z + 1] * (1 + (z == 1)),
          expanded_state[y + 1, x    , z + 1] * (1 + (z == 1)),
          expanded_state[y - 1, x + 1, z + 1] * (1 + (z == 1)),
          expanded_state[y    , x + 1, z + 1] * (1 + (z == 1)),
          expanded_state[y + 1, x + 1, z + 1] * (1 + (z == 1)),
          na.rm = TRUE
        )
        new_state[y, x, z] <- is.element(
          sum(neighbours),
          c(if (expanded_state[y, x, z]) 2, 3)
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
