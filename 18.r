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
flat_parse <- function(tokens, verbose) {
  if (verbose)
    cat(paste("flat-parsing", paste(rev(tokens), collapse = " ")), sep = "\n")
  if (length(tokens) == 1)
    return(suppressWarnings(as.numeric(tokens)))
  pluses <- tokens == "+"
  if (all(!pluses))
    return(eval(parse(text = paste(tokens, collapse = " "))))
  plus_index <- which(pluses)[1]
  new_tokens <- c(
    tokens[-((plus_index - 1):length(tokens))],
    as.character(sum(as.numeric(tokens[c(plus_index - 1, plus_index + 1)]))),
    tokens[-(1:(plus_index + 1))]
  )
  flat_parse(new_tokens, verbose)
}
iterate2 <- function(tokens, verbose) {
  if (anyNA(tokens))
    stop(print(tokens))
  if (verbose)
    cat(paste("parsing", paste(rev(tokens), collapse = " ")), sep = "\n")
  rbracs <- tokens == ")"
  if (all(!rbracs))
    return(flat_parse(tokens, verbose))
  start <- which(rbracs)[1]
  bracket_balance <- cumsum((tokens == ")") - (tokens == "("))
  end <- start + which(bracket_balance[-(1:start)] < bracket_balance[start])[1]
  sub_tokens <- tokens[(start + 1):(end - 1)]
  new_tokens <- c(
    tokens[-(start:length(tokens))],
    as.character(iterate2(sub_tokens, verbose)),
    tokens[-(1:end)]
  )
  iterate2(new_tokens, verbose)
}
parse2 <- function(tokens, verbose = FALSE) {
  iterate2(rev(tokens), verbose)
}
test_res2 <- c(231, 51, 46, 1445, 669060, 23340)
stopifnot(all(vapply(test_tokens, parse2, numeric(1)) == test_res2))
format(sum(vapply(x, parse2, numeric(1))), scientific = FALSE) # part two: 231235959382961
