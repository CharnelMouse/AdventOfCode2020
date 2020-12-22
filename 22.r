x <- readLines("22.txt")
brk <- which(x == "")
p1_deck <- strtoi(x[2:(brk - 1)])
p2_deck <- strtoi(x[(brk + 2):length(x)])
play <- function(p1, p2) {
  len1 <- length(p1)
  len2 <- length(p2)
  n_rounds <- min(len1, len2)
  if (n_rounds == 0)
    return(if (len2 == 0) list(1L, p1) else list(2L, p2))
  p1_wins <- p1[1:n_rounds] > p2[1:n_rounds]
  new_p1 <- c(
    p1[-(1:n_rounds)],
    if (any(p1_wins))
      c(p1[which(p1_wins)], p2[which(p1_wins)])[order(rep(1:sum(p1_wins), 2))]
  )
  new_p2 <- c(
    p2[-(1:n_rounds)],
    if (any(!p1_wins))
      c(p2[which(!p1_wins)], p1[which(!p1_wins)])[order(rep(1:sum(!p1_wins), 2))]
  )
  if (any(is.na(new_p1)) || any(is.na(new_p2)))
    stop(paste(toString(p1), toString(p2), toString(new_p1), toString(new_p2), sep = "\n"))
  play(new_p1, new_p2)
}
res <- play(p1_deck, p2_deck)

score <- function(deck) {
  len <- length(deck)
  sum(len:1 * deck)
}
score(res[[2]]) # part one: 34255

rec_acc <- function(p1_deck, p2_deck, depth) {
  since <- 0
  p1 <- p1_deck
  p2 <- p2_deck
  history <- list()
  len1 <- length(p1)
  len2 <- length(p2)
  while (len1 > 0 && len2 > 0) {
    cat(
      "depth", depth,
      "cards", sum(length(p1), length(p2)),
      "history length after last subgame", since,
      "history length", length(history),
      "\r"
      )
    if (any(vapply(
      history,
      identical,
      logical(1),
      list(p1, p2)
    ))) {
      p2 <- integer()
      len2 <- 0
      break
    }
    history <- c(history, list(list(p1, p2)))
    if (len1 > p1[1] && len2 > p2[1]) {
      sub_result <- rec_acc(
        p1[1 + 1:p1[1]],
        p2[1 + 1:p2[1]],
        depth + 1
      )
      since <- length(history)
      p1_wins <- sub_result[[1]] == 1
    }else{
      p1_wins <- p1[1] > p2[1]
    }
    new_p1 <- c(
      p1[-1],
      if (p1_wins) c(p1[1], p2[1])
    )
    new_p2 <- c(
      p2[-1],
      if (!p1_wins) c(p2[1], p1[1])
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
recursive_play <- function(p1, p2) {
  rec_acc(p1, p2, 1)
}
res2 <- recursive_play(p1_deck, p2_deck)
score(res2[[2]]) # part two: 33369
