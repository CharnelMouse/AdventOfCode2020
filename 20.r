x <- readLines("20.txt")
bps <- which(x == "")
height <- bps[1] - 2L
width <- nchar(x[2])
stopifnot(height == width)
starts <- c(1, bps + 1)
tile_ids <- strtoi(substr(x[starts], 6, 9))
contents <- do.call(rbind, strsplit(x[-c(bps, starts)], "", fixed = TRUE))
# tile x row x column
tiles <- aperm(
  array(
    contents,
    c(height, length(contents)/(height*width), width),
    dimnames = list(NULL, tile_ids, NULL)
  ),
  c(2, 1, 3),
)
edge_to_int <- function(edge) {
  bools <- edge == "#"
  max(
    sum(2^((width - 1):0)*bools),
    sum(2^((width - 1):0)*rev(bools))
  )
}
edge_ids <- apply(
  tiles,
  1,
  function(tile) c(
    edge_to_int(tile[1, ]),
    edge_to_int(tile[height, ]),
    edge_to_int(tile[, 1]),
    edge_to_int(tile[, width])
  )
)
edge_freqs <- table(edge_ids)
stopifnot(edge_freqs <= 2) ## All edges unique
border_edges <- strtoi(names(edge_freqs)[edge_freqs == 2])
is_corner <- apply(
  edge_ids,
  2,
  function(ids) sum(is.element(ids, border_edges)) == 2
)
format(
  prod(strtoi(names(is_corner[is_corner]))),
  scientific = FALSE
) # part one: 68781323018729
