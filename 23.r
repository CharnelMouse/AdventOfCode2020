x <- strtoi(strsplit(readLines("23.txt"), "")[[1]])
n_cups <- length(x)
mod <- function(n) (n - 1) %% n_cups + 1

choose_dest <- function(current, picked) {
  n <- mod(current - 1)
  if (n %in% picked)
    choose_dest(current - 1, picked)
  else
    n
}
move <- function(state) {
  cups <- state[[1]]
  current_id <- state[[2]]
  current_val <- cups[current_id]
  pick_ids <- mod(current_id + 1:3)
  pick_vals <- cups[pick_ids]
  nonpicked <- cups[-pick_ids]
  destination_val <- choose_dest(current_val, pick_vals)
  destination_id <- match(destination_val, nonpicked)
  new_cups <- c(
    nonpicked[1:destination_id],
    pick_vals,
    nonpicked[-(1:destination_id)]
  )
  list(
    new_cups,
    mod(match(current_val, new_cups) + 1)
  )
}
str <- function(cups) {
  start <- match(1, cups)
  paste(c(cups[-(1:start)], cups[0:(start - 1)]), collapse = "")
}
play <- function(cups, n, ...) {
  Reduce(function(x, f) f(x), replicate(n, move), init = list(cups, 1), ...)
}
str(play(x, 100)[[1]]) # part one: 29385746
