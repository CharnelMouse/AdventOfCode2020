x <- readLines("24.txt")
moves <- strsplit(
  mapply(
    function(str) {
      gsub("w", "w,", gsub("e", "e,", str, fixed = TRUE), fixed = TRUE)
    },
    x,
    USE.NAMES = FALSE
  ),
  ",",
  fixed = TRUE
)
position <- function(moves) {
  x <- sum(is.element(moves, c("e", "se")) - is.element(moves, c("w", "nw")))
  y <- sum(startsWith(moves, "n") - startsWith(moves, "s"))
  complex(1, x, y)
}
positions <- mapply(position, moves)
black <- as.complex(names(table(positions))[table(positions) %% 2 == 1])
length(black) # part one: 400

# part two: convert positions to grid notation,
# then use GoL code from day 11

starting_x_range <- range(Re(positions))
starting_y_range <- range(Im(positions))
blank <- matrix(
  FALSE,
  nrow = 1L + diff(starting_y_range),
  ncol = 1L + diff(starting_x_range)
)
start <- Reduce(
  function(mat, pos) {
    `[<-`(
      mat,
      -starting_y_range[1] + 1 + Im(pos),
      -starting_x_range[1] + 1 + Re(pos),
      TRUE
    )
  },
  black,
  init = blank
)

run <- function(state, iter) {
  ylen <- nrow(state)
  xlen <- ncol(state)
  while (iter > 0) {
    expand_yl <- any(state[1, ])
    expand_yr <- any(state[ylen, ])
    expand_xl <- any(state[, 1])
    expand_xr <- any(state[, xlen])
    new_ylen <- ylen + expand_yl + expand_yr
    new_xlen <- xlen + expand_xl + expand_xr
    new_lens <- c(new_ylen, new_xlen)
    # add empty cells at end of dims to prevent errors in neighbour lookups
    expanded_state <- array(
      FALSE,
      dim = new_lens + 1
    )
    expanded_state[expand_yl + 1:ylen, expand_xl + 1:xlen] <- state
    new_state <- array(FALSE, new_lens)
    for (y in 1:new_ylen) {
      for (x in 1:new_xlen) {
        neighbours <- sum(
          c(
            expanded_state[y - 1, x + 0:1],
            expanded_state[y    , x + -1:1],
            expanded_state[y + 1, x + -1:0]
          )
        )
        new_state[y, x] <- is.element(
          neighbours,
          c(2, if (expanded_state[y, x]) 3)
        )
      }
    }
    state <- new_state
    ylen <- new_ylen
    xlen <- new_xlen
    iter <- iter - 1
  }
  state
}
final <- run(start, 100)
sum(final) # part two: 3768
