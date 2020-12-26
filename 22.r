x <- readLines("22.txt")
brk <- which(x == "")
p1_deck <- strtoi(x[2:(brk - 1)])
p2_deck <- strtoi(x[(brk + 2):length(x)])
game <- function(p1_deck, p2_deck, depth, recurse, top) {
  p1 <- p1_deck
  p2 <- p2_deck
  len1 <- length(p1)
  len2 <- length(p2)
  p1_history <- list()
  p2_history <- list()
  p1_lenhist <- integer()
  p2_lenhist <- integer()
  while (len1 > 0 && len2 > 0) {
    lenhist_matches <- p1_lenhist == len1 & p2_lenhist == len2
    p1_histmatch <- vapply(
      p1_history[lenhist_matches],
      function(x) all(x == p1),
      logical(1)
    )
    if (
      any(p1_histmatch) &&
      any(vapply(
        p2_history[lenhist_matches][p1_histmatch],
        function(x) all(x == p2),
        logical(1)
      ))
    ) {
      len2 <- 0
      break
    }
    p1_history <- c(p1_history, list(p1))
    p2_history <- c(p2_history, list(p2))
    p1_lenhist <- c(p1_lenhist, len1)
    p2_lenhist <- c(p2_lenhist, len2)
    if (recurse && len1 > p1[1] && len2 > p2[1]) {
      sub_result <- game(
        p1[1 + 1:p1[1]],
        p2[1 + 1:p2[1]],
        depth + 1,
        recurse,
        FALSE
      )
      p1_wins_round <- sub_result == 1
    }else{
      p1_wins_round <- p1[1] > p2[1]
    }
    new_p1 <- c(
      p1[-1],
      if (p1_wins_round) c(p1[1], p2[1])
    )
    new_p2 <- c(
      p2[-1],
      if (!p1_wins_round) c(p2[1], p1[1])
    )
    p1 <- new_p1
    p2 <- new_p2
    len1 <- length(p1)
    len2 <- length(p2)
  }
  if (top) {
    if (len2 == 0)
      p1
    else
      p2
  }else{
    if (len2 == 0)
      1L
    else
      2L
  }
}
play <- function(p1, p2, recurse) {
  game(p1, p2, 1, recurse, TRUE)
}
score <- function(deck) {
  len <- length(deck)
  sum(len:1 * deck)
}

res <- play(p1_deck, p2_deck, FALSE)
score(res) # part one: 34255
res2 <- play(p1_deck, p2_deck, TRUE)
score(res2) # part two: 33369
