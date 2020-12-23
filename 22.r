# warning: this takes about 15 mins to run on my input
x <- readLines("22.txt")
brk <- which(x == "")
p1_deck <- strtoi(x[2:(brk - 1)])
p2_deck <- strtoi(x[(brk + 2):length(x)])
game <- function(p1_deck, p2_deck, depth, recurse) {
  since <- 0
  p1 <- p1_deck
  p2 <- p2_deck
  len1 <- length(p1)
  len2 <- length(p2)
  history <- list()
  while (len1 > 0 && len2 > 0) {
    if (any(vapply(
      history,
      identical,
      logical(1),
      list(p1, p2)
    ))) {
      len2 <- 0
      break
    }
    history <- c(history, list(list(p1, p2)))
    if (recurse && len1 > p1[1] && len2 > p2[1]) {
      sub_result <- game(
        p1[1 + 1:p1[1]],
        p2[1 + 1:p2[1]],
        depth + 1,
        recurse
      )
      since <- length(history)
      p1_wins_round <- sub_result[[1]] == 1
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
  if (len2 == 0)
    list(1L, p1)
  else
    list(2L, p2)
}
play <- function(p1, p2, recurse) {
  game(p1, p2, 1, recurse)
}
score <- function(deck) {
  len <- length(deck)
  sum(len:1 * deck)
}

res <- play(p1_deck, p2_deck, FALSE)
score(res[[2]]) # part one: 34255
res2 <- play(p1_deck, p2_deck, TRUE)
score(res2[[2]]) # part two: 33369
