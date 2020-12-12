x <- readLines("12.txt")
directions <- substr(x, 1, 1)
amounts <- as.integer(substring(x, 2))
compass <- c("N", "E", "S", "W")
# all turns are multiples of 90 degrees, so use NESW for facing
convert_relative <- function(directions, amounts, facing) {
  facing_enum <- match(facing, compass) - 1L
  relative_turns <- (directions == "R") - (directions == "L")
  facings <- cumsum(c(facing_enum, relative_turns*amounts/90))[-1] %% 4
  directions[directions == "F"] <- compass[facings[directions == "F"] + 1]
  directions
}
run <- function(directions, amounts, facing) {
  abs_directions <- convert_relative(directions, amounts, facing)
  horizontal <- sum(amounts[abs_directions == "E"]) - sum(amounts[abs_directions == "W"])
  vertical <- sum(amounts[abs_directions == "N"]) - sum(amounts[abs_directions == "S"])
  c(horizontal, vertical)
}
sum(abs(run(directions, amounts, "E"))) # part one: 2270
rotate <- function(pos, times_cc) {
  switch(
    as.integer(times_cc),
    c(-pos[2], pos[1]),
    -pos,
    c(pos[2], -pos[1])
  )
}
way_run <- function(directions, amounts, way_start, ship_start) {
  if (length(directions) == 0)
    return(ship_start)
  dir <- directions[1]
  am <- amounts[1]
  step <- function(x, y) way_run(directions[-1], amounts[-1], x, y)
  switch(
    dir,
    N = step(way_start + c(0, am), ship_start),
    E = step(way_start + c(am, 0), ship_start),
    S = step(way_start + c(0, -am), ship_start),
    W = step(way_start + c(-am, 0), ship_start),
    L = step(rotate(way_start, am/90), ship_start),
    R = step(rotate(way_start, 4 - am/90), ship_start),
    F = step(way_start, ship_start + way_start*am)
  )
}
sum(abs(way_run(directions, amounts, c(10, 1), c(0, 0)))) # part two: 138669
