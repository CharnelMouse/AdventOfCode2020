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
