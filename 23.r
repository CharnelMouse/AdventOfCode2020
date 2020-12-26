x <- strtoi(strsplit(readLines("23.txt"), "")[[1]])
str_acc <- function(current, cup_links) {
  n <- cup_links[current[1]]
  if (n == 1)
    paste(rev(current), collapse = "")
  else
    str_acc(c(n, current), cup_links)
}
str <- function(cup_links) {
  start <- cup_links[1]
  str_acc(start, cup_links)
}
# choosing destination within a function makes this last 7.5 hours!
play <- function(cups, n) {
  cup_links <- integer(length(cups))
  cup_links[cups] <- c(cups[-1], cups[1])
  current_val <- cups[1]
  n_cups <- length(cups)
  while (n > 0) {
    pick1 <- cup_links[current_val]
    pick2 <- cup_links[pick1]
    pick3 <- cup_links[pick2]
    destination <- current_val - 1
    while (
      destination == pick1 ||
      destination == pick2 ||
      destination == pick3 ||
      destination == 0
    ) {
      if (destination == 0)
        destination <- n_cups
      else
        destination <- destination - 1
    }
    next_val <- cup_links[pick3]
    cup_links[current_val] <- next_val
    cup_links[pick3] <- cup_links[destination]
    cup_links[destination] <- pick1
    current_val <- next_val
    n <- n - 1
  }
  cup_links
}
res1 <- play(x, 100)
str(res1) # part one: 29385746
expanded_cups <- c(x, (length(x) + 1):1000000)
res2 <- play(expanded_cups, 10000000)
first <- res2[1]
as.numeric(first)*as.numeric(res2[first]) # part two: 680435423892
