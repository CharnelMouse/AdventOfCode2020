# this version is much slower, but is (nearly) generalised
# enough for both parts to use the same function
x <- do.call(rbind, strsplit(readLines("17.txt"), "", fixed = TRUE)) == "#"
chars <- function(x) ifelse(x, "#", ".")
sub_list <- function(arr, inds_list) {
  # programmatic version of arr[inds_list[[1]], inds_list[[2]], ...]
  do.call(`[`, c(list(arr), inds_list))
}
sub_list_to <- function(arr, inds_list, vals) {
  # programmatic version of `<-`(arr, inds_list[[1]], inds_list[[2]], ..., vals)
  do.call(
    `[<-`,
    c(
      arr,
      inds_list,
      list(vals)
    )
  )
}
any_dimind <- function(arr, dim, ind, n_dim) {
  any(sub_list(arr, `[<-`(rep(list(TRUE), n_dim), dim, ind)))
}
update <- function(pos, expanded_state) {
  neighbours <- sub_list(expanded_state, lapply(pos, `+`, (-1):1))
  # neighbours lengths varies depending on whether on left-hand boundary,
  # since e.g. x[0:2] returns an two-length value
  ######## last dependences on dim length here! ########
  left_bound <- pos == 1
  base_len <- 9 - 3*sum(left_bound[1:2]) + all(left_bound[1:2])
  weights <- rep(
    c(1, 1 + left_bound[3]),
    c(base_len*(2 - left_bound[3]), base_len)
  )
  weighted_sum <- sum(neighbours*weights)
  is.element(
    weighted_sum,
    c(3, if (expanded_state[pos[1], pos[2], pos[3]]) 4)
  )
}
update2 <- function(pos, expanded_state) {
  neighbours <- sub_list(expanded_state, lapply(pos, `+`, (-1):1))
  # neighbours lengths varies depending on whether on left-hand boundary,
  # since e.g. x[0:2] returns an two-length value
  ######## last dependences on dim length here! ########
  left_bound <- pos == 1
  base_len <- 9 - 3*sum(left_bound[1:2]) + all(left_bound[1:2])
  xyz_weights <- rep(
    c(1, 1 + left_bound[3]),
    c(base_len*(2 - left_bound[3]), base_len)
  )
  xyz_len <- length(xyz_weights)
  weights <- rep(xyz_weights, 3 - left_bound[4]) *
    rep(
      c(1, 1 + left_bound[4]),
      c(xyz_len*(2 - left_bound[4]), xyz_len)
    )
  weighted_sum <- sum(neighbours*weights)
  is.element(
    weighted_sum,
    c(3, if (expanded_state[pos[1], pos[2], pos[3], pos[4]]) 4)
  )
}
# added dims are symmetric, so needn't track their strictly negative indices
# (also interchangeable, but I'm not sure how to make use of this)
iterate <- function(state, lens, iter, update_fn) {
  if (iter == 0)
    return(state)
  n_dim <- length(lens)
  expand_left <- c(
    any_dimind(state, 1, 1, n_dim),
    any_dimind(state, 2, 1, n_dim),
    rep(FALSE, n_dim - 2)
  )
  expand_right <- vapply(
    1:n_dim,
    function(n) any_dimind(state, n, lens[n], n_dim),
    logical(1)
  )
  new_lens <- lens + expand_left + expand_right
  # add empty cells at end of dims to prevent errors in neighbour lookups
  expanded_state <- sub_list_to(
    list(array(FALSE, new_lens + 1)),
    Map(function(exp, len) exp + 1:len, expand_left, lens),
    state
  )
  new_state <- array(
    # getting neighbours for each cell separately,
    # so lots of repeated fetching of each cell value
    apply(
      stop(print(unname(do.call(expand.grid, lapply(new_lens, seq.int))))),
      1,
      update_fn,
      expanded_state
    ),
    new_lens
  )
  iterate(new_state, new_lens, iter - 1, update_fn)
}
run <- function(state, iter, add, update_fn) {
  dims <- c(dim(state), rep(1, add))
  start <- array(state, dims)
  iterate(start, dims, iter, update_fn)
}
sum_with_syms <- function(arr, sym_dims) {
  vals <- lapply(
    seq_along(dim(arr)),
    function(n) if (is.element(n, sym_dims)) list(TRUE, -1) else TRUE
  )
  sum(apply(expand.grid(vals), 1, function(x) sum(sub_list(arr, x))))
}
sum_with_syms(run(x, 6, 1, update), 3) # part one: 336
sum_with_syms(run(x, 6, 2, update2), 3:4) # part two: 2620
