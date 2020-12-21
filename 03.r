x <- readLines("03.txt")
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
trees(x, 3, 1) # part one: 218
all_trees <- mapply(
  trees,
  x_move = c(1, 3, 5, 7, 1),
  y_move = c(1, 1, 1, 1, 2),
  MoreArgs = list(map = x)
)
prod(all_trees) # part two: 3847183340
