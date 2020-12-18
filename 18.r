# Had to rewrite halfway through, due to bad tokenisation.
inp <- readLines("18.txt")
tokenise <- function(strs) {
  lapply(
    strsplit(strs, "", fixed = TRUE),
    function(chars) chars[chars != " "]
  )
}
x <- tokenise(inp)
tests <- c(
  "1 + 2 * 3 + 4 * 5 + 6",
  "1 + (2 * 3) + (4 * (5 + 6))",
  "2 * 3 + (4 * 5)",
  "5 + (8 * 3 + 9 + 3 * 4 * 3)",
  "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
  "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
)
test_tokens <- tokenise(tests)
iterate <- function(tokens, verbose) {
  if (verbose)
    cat(paste("parsing", paste(rev(tokens), collapse = " ")), sep = "\n")
  first <- tokens[1]
  if (is.na(first))
    stop(paste(rev(tokens)))
  int <- suppressWarnings(as.numeric(first))
  if (!is.na(int)) {
    if (length(tokens) == 1) {
      if (verbose)
        cat(paste("returning", int), sep = "\n")
      int
    }else{
      op <- get(tokens[2])
      op(int, iterate(tokens[-c(1, 2)], verbose))
    }
  }else{
    if (first != ")")
      stop("unexpected token")
    bracket_balance <- cumsum((tokens == ")") - (tokens == "("))
    closing_index <- 1 + which(bracket_balance[-1] == 0)[1]
    if (closing_index == 2L) {
      iterate(tokens[-(1:2)], verbose)
    }else{
      bracket_val <- iterate(tokens[2:(closing_index - 1)], verbose)
      iterate(c(bracket_val, tokens[-(1:closing_index)]), verbose)
    }
  }
}
parse1 <- function(tokens, verbose = FALSE) {
  if (verbose) cat("starting", sep = "\n")
  iterate(rev(tokens), verbose)
}
test_res1 <- c(71, 51, 26, 437, 12240, 13632)
stopifnot(all(vapply(test_tokens, parse1, numeric(1)) == test_res1))
format(sum(vapply(x, parse1, numeric(1))), scientific = FALSE) # part one: 8929569623593
test_res2 <- c(231, 51, 46, 1445, 669060, 23340)
