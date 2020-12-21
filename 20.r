x <- readLines("20.txt")
bps <- which(x == "")
tile_len <- nchar(x[2])
height <- bps[1] - 2L
stopifnot(height == tile_len)
starts <- c(1, bps + 1)
tile_ids <- strtoi(substr(x[starts], 6, 9))
n_tiles <- length(tile_ids)
contents <- do.call(rbind, strsplit(x[-c(bps, starts)], "", fixed = TRUE))
# tile x row x column
tiles <- aperm(
  array(
    contents,
    c(tile_len, length(contents)/tile_len^2, tile_len),
    dimnames = list(NULL, tile_ids, NULL)
  ),
  c(2, 1, 3),
)

# edge listing ------------------------------------------------------------

# reverse edge order for left and bottom, to make edges rotation-invariant
edge_to_int <- function(edge) {
  bools <- edge == "#"
  as.integer(sum(2^((tile_len - 1):0)*bools))
}
tile_edges <- function(tile) c(
  edge_to_int(tile[1, ]),
  edge_to_int(tile[, tile_len]),
  edge_to_int(rev(tile[tile_len, ])),
  edge_to_int(rev(tile[, 1]))
)
edges <- apply(tiles, 1, tile_edges)
# array of edges with and without horizontal flipping
flip_edge <- function(edge) {
  bools <- outer(
    edge,
    (tile_len - 1):0,
    function(edge, mult) (edge %/% (as.integer(2^mult))) %% 2L
  )
  apply(bools, 1, function(b) sum(2^((tile_len - 1):0)*rev(b)))
}
flip_pos <- function(pos) c(1, 4, 3, 2)[pos]
edge_array <- function(edges) {
  flipped_edges <- matrix(
    flip_edge(as.integer(edges)),
    nrow = nrow(edges)
  )[flip_pos(1:4), ]
  array(c(edges, flipped_edges), dim = c(dim(edges), 2))
}
# array with edge information, which we'll use to place tiles
# by walking along matching pairs.
# 1: position of edge (NESW -> 1:4) to rotate to face south (after any flip)
# 2: tile
# 3: 1 if no flip, 2 if horizontal flip (before rotations)
edge_arr <- edge_array(edges)

# corner finding ----------------------------------------------------------

edge_freqs <- table(edge_arr)
stopifnot(all(table(edge_arr) <= 2)) ## All edges unique
border_edges <- strtoi(names(edge_freqs)[edge_freqs == 2])
is_corner <- apply(
  edge_arr[, , 1],
  2,
  function(ids) sum(is.element(ids, border_edges)) == 2
)
corner_ids <- tile_ids[is_corner]
format(prod(strtoi(corner_ids)), scientific = FALSE) # part one: 68781323018729

# puzzle solving ----------------------------------------------------------

# one-based modular arithmetic, to make position cycling easier
mod1 <- function(x, mod) (x - 1) %% mod + 1
# start with corner tile in top left
start_tile <- which(is_corner)[1]
start_tile_edges <- edge_arr[, start_tile, 1]
start_tile_edge_freq <- table(edge_arr)[as.character(start_tile_edges)]
start_corner_indices <- which(start_tile_edge_freq == 1)
start_bottom_pos <-
  if (diff(start_corner_indices) == 1) {
    unname(mod1(start_corner_indices[2] + 2, 4))
  }else{
    3
  }
left_sol_start_val <- start_tile_edges[start_bottom_pos]
left_sol_start_ind <- array(
  c(start_bottom_pos, start_tile, 1),
  c(1, 3)
)
edge_travel <- function(inds, val, edge_arr) {
  matches <- which(edge_arr == flip_edge(val), arr.ind = TRUE)
  if (nrow(matches) == 1) return(inds[rev(1:nrow(inds)), , drop = FALSE])
  if (nrow(matches) != 2) stop("multiple edge matches")
  expected_copy <- c(
    flip_pos(inds[1, 1]),
    inds[1, 2],
    3 - inds[1, 3]
  )
  if (!any(apply(matches, 1, function(x) all(x == expected_copy))))
    stop("incorrect match")
  new_match <- matches[
    apply(
      matches, 1,
      function(x) x[2] != expected_copy[2]
    ),
    ]
  if (length(new_match) != 3)
    stop("multiple matches")
  new_ind <- c(mod1(new_match[1] + 2, 4), new_match[2:3])
  if (any(new_ind > dim(edge_arr)))
    stop(paste("invalid indices:", toString(new_ind)))
  new_val <- edge_arr[new_ind[1], new_ind[2], new_ind[3]]
  edge_travel(unname(rbind(new_ind, inds)), new_val, edge_arr)
}

# LHS tiles.
left_sol_inds <- edge_travel(left_sol_start_ind, left_sol_start_val, edge_arr)

# Now switch to matching tiles horizontally.
# For each row, start with LHS tile in that row.
# We have the position for the bottom edge, and need the right-hand one.
right_sol_start_inds <- `[<-`(
  left_sol_inds, TRUE, 1,
  mod1(left_sol_inds[, 1] - 1, 4)
)
right_start_edges <- apply(
  right_sol_start_inds,
  1,
  function(x) edge_arr[mod1(x[1] + 0:3, 4), x[2], x[3]]
)

row_sols <- lapply(
  1:nrow(right_sol_start_inds),
  function(n) {
    ind <- right_sol_start_inds[n, , drop = FALSE]
    val <- edge_arr[ind[1], ind[2], ind[3]]
    edge_travel(ind, val, edge_arr)
  }
)

# combine tiles into final map
rotate_charmap_to_right <- function(pos, charmap) {
  height <- nrow(charmap)
  width <- ncol(charmap)
  stopifnot(height == width)
  switch(
    pos,
    t(charmap)[1:width, height:1],
    charmap,
    t(charmap)[width:1, 1:height],
    charmap[height:1, width:1]
  )
}
final_tile <- function(inds) {
  pos <- inds[1]
  tile <- inds[2]
  flip <- inds[3]
  tile_map <- tiles[tile, , ]
  if (flip == 2) {
    tile_map <- tile_map[1:tile_len, tile_len:1]
  }
  rotated <- rotate_charmap_to_right(pos, tile_map)
  rotated[2:(tile_len - 1), 2:(tile_len - 1)] # remove edges
}
rows <- lapply(
  row_sols,
  function(row_inds) {
    tiles <- lapply(
      1:nrow(row_inds),
      function(n) {
        final_tile(row_inds[n, ])
      }
    )
    do.call(cbind, tiles)
  }
)
final_image <- do.call(rbind, rows)

# monster hunting ---------------------------------------------------------

monster_pattern <- c(
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "
)
monster_width <- nchar(monster_pattern[1])
monster_height <- length(monster_pattern)
monster_chars <- do.call(rbind, strsplit(monster_pattern, "", fixed = TRUE))
monster_pos <- which(monster_chars == "#", arr.ind = TRUE)
is_monster_top_left <- function(row, col, image) {
  if (
    row + monster_height - 1 > nrow(image) ||
    col + monster_width - 1 > ncol(image)
  )
    FALSE
  else{
    image_section <- image[
      row:(row + monster_height - 1),
      col:(col + monster_width - 1)
      ]
    all(image_section[monster_chars == "#"] == "#")
  }
}
monster_starts <- function(image) {
  outer(
    1:nrow(image),
    1:ncol(image),
    Vectorize(function(row, col) is_monster_top_left(row, col, image))
  )
}
final_image_flip <- final_image[1:nrow(final_image), ncol(final_image):1]
possible_orientations <- list(
  rotate_charmap_to_right(1, final_image),
  rotate_charmap_to_right(2, final_image), # original
  rotate_charmap_to_right(3, final_image),
  rotate_charmap_to_right(4, final_image),
  rotate_charmap_to_right(1, final_image_flip),
  rotate_charmap_to_right(2, final_image_flip),
  rotate_charmap_to_right(3, final_image_flip),
  rotate_charmap_to_right(4, final_image_flip)
)
orientation_monster_start_positions <- vapply(
  possible_orientations,
  monster_starts,
  matrix(logical(1), nrow = nrow(final_image), ncol = ncol(final_image))
)
orientation_contains_monsters <- apply(
  orientation_monster_start_positions,
  3, any
)
stopifnot(sum(orientation_contains_monsters) == 1)
used_orientation <- which(orientation_contains_monsters)
used_image <- possible_orientations[[used_orientation]]
monster_start_positions <-  which(
  orientation_monster_start_positions[, , used_orientation],
  arr.ind = TRUE
)
monster_cell_positions <- which(monster_chars == "#", arr.ind = TRUE)
# unique in case monsters overlap
monster_char_positions <- unique(
  do.call(
    rbind,
    lapply(
      1:nrow(monster_cell_positions),
      function(n) {
        rep(
          monster_cell_positions[n, ],
          each = nrow(monster_start_positions)
        ) + monster_start_positions - 1
      }
    )
  )
)
monsterless_image <- Reduce(
  function(img, n) `[<-`(
    img,
    monster_char_positions[n, 1], monster_char_positions[n, 2],
    "O"
  ),
  1:nrow(monster_char_positions),
  init = used_image
)
sum(monsterless_image == "#") # part two: 1629
# final image
# cat(apply(monsterless_image, 1, paste, collapse = ""), sep = "\n")
