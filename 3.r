x <- readLines("3.txt")
width <- nchar(x[1])
height <- length(x)

# Part one: 3 right, 1 down, until reach end, return number of trees hit
# Start in zero indices, to make thinking in mod easier

cols <- cumsum(rep(3, height - 1)) %% width
squares <- mapply(
  function(str, n) {
    substr(str, n + 1, n + 1)
  },
  x,
  c(0L, cols),
  USE.NAMES = FALSE
)
sum(squares == "#")

# Tidy version

x <- readLines("3.txt")
trees <- function(map, x_move, y_move) {
  height <- length(map)
  width <- nchar(map[1])
  n_moves <- (height - 1)/y_move
  # Work out indices starting from 0 to make mod maths easier
  rows <- cumsum(c(0, rep(y_move, n_moves)))
  cols <- cumsum(c(0, rep(x_move, n_moves))) %% width
  squares <- mapply(
    function(row_str, col) substr(row_str, col, col),
    # Convert to from-one indexing here
    map[rows + 1L],
    cols + 1L
  )
  sum(squares == "#")
}
stopifnot(trees(x, 3, 1) == sum(squares == "#"))

# Part two: more slopes
# 1:1, 3:1 (part one), 5:1, 7:1, 1:2

all_trees <- mapply(
  trees,
  x_move = c(1, 3, 5, 7, 1),
  y_move = c(1, 1, 1, 1, 2),
  MoreArgs = list(map = x)
)
prod(all_trees)

# All together

x <- readLines("3.txt")
trees <- function(map, x_move, y_move) {
  n_moves <- (length(map) - 1)/y_move
  rows <- cumsum(c(0, rep(y_move, n_moves)))
  cols <- cumsum(c(0, rep(x_move, n_moves))) %% nchar(map[1])
  squares <- mapply(
    function(row_str, col) substr(row_str, col, col),
    map[rows + 1L], cols + 1L
  )
  sum(squares == "#")
}
trees(x, 3, 1)
all_trees <- mapply(
  trees,
  x_move = c(1, 3, 5, 7, 1),
  y_move = c(1, 1, 1, 1, 2),
  MoreArgs = list(map = x)
)
prod(all_trees)
