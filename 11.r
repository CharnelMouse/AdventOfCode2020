# Interesting day, so I'll add more comments than usual.
x <- do.call(rbind, strsplit(readLines("11.txt"), ""))
# Effectively game of life, but the "walls" make a matrix-based approach
# inefficient. Instead, I'll get the position of each seat, and derive
# neighbours, so I'm working off of graph edge information.
seats <- which(x != ".", arr.ind = TRUE)
n_seats <- nrow(seats)
values <- rep(0L, n_seats)
directions <- expand.grid(x = -1:1, y = -1:1)[-5,]
neighbours <- lapply(
  seq.int(n_seats),
  function(n) {
    pos <- seats[n, ]
    xdiff <- abs(seats[, 1] - pos[1])
    ydiff <- abs(seats[, 2] - pos[2])
    which(pmax.int(xdiff, ydiff) <= 1 & (xdiff + ydiff != 0))
  }
)
neighbour_totals <- function(values, neighbours) {
  vapply(
    neighbours,
    function(ids) sum(values[ids]),
    integer(1)
  )
}
update <- function(values, neighbours, breaks, changed) {
  totals <- neighbour_totals(values, neighbours[changed])
  rules <- findInterval(totals, breaks)
  values[changed] <- ifelse(rules == 3, 0L, ifelse(rules == 1, 1L, values[changed]))
  values
}
run <- function(values, neighbours, rule_breaks, changed) {
  new <- update(values, neighbours, rule_breaks, changed)
  changed_seats <- which(new != values)
  if (length(changed_seats) == 0)
    values
  else
    run(new, neighbours, rule_breaks, changed_seats)
}
sum(run(values, neighbours, c(0, 1, 4), seq.int(n_seats))) # part one: 2361
# Thankfully, the above ports over to part two well, we just need to change the
# neighbours and the activation rules.
xmax <- max(seats[, 1])
ymax <- max(seats[, 2])
first_neighbour <- function(pos, seats, direction) {
  check <- pos + direction
  if (check[1] <= 0 || check[2] <= 0 || check[1] > xmax || check[2] > ymax)
    return(integer())
  present <- which(seats[, 1] == check[1] & seats[, 2] == check[2])
  if (length(present) == 1)
    present
  else
    first_neighbour(check, seats, direction)
}
# Calculating neighbours is very slow, I need to find a better method.
far_neighbours <- lapply(
  seq.int(n_seats),
  function(n) {
    unlist(
      lapply(
        seq.int(nrow(directions)),
        function(m) first_neighbour(seats[n, ], seats, unlist(directions[m, ]))
      )
    )
  }
)
sum(run(values, far_neighbours, c(0, 1, 5), seq.int(n_seats))) # part two: 2119
