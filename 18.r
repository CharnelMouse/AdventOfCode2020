x <- strsplit(readLines("18.txt"), " ", fixed = TRUE)
tests <- c(
  "1 + 2 * 3 + 4 * 5 + 6",
  "1 + (2 * 3) + (4 * (5 + 6))",
  "2 * 3 + (4 * 5)",
  "5 + (8 * 3 + 9 + 3 * 4 * 3)",
  "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
  "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
)
tests_tokens <- strsplit(tests, " ", fixed = TRUE)
iterate <- function(tokens, verbose) {
  if (verbose) cat(paste("parsing", paste(rev(tokens), collapse = " ")), sep = "\n")
  first <- tokens[1]
  if (is.na(first))
    stop(paste(rev(tokens)))
  int <- suppressWarnings(as.numeric(first))
  if (length(tokens) == 1) {
    if (verbose) cat(paste("returning", int), sep = "\n")
    return(as.numeric(int))
  }
  if (!is.na(int)) {
    op <- get(tokens[2])
    op(int, iterate(tokens[-c(1, 2)], verbose))
  }else{
    if (!endsWith(first, ")"))
      stop(first)
    bracket_balance <- cumsum(vapply(
      strsplit(tokens, "", fixed = TRUE),
      function(chars) sum(chars == ")") - sum(chars == "("),
      numeric(1)
    ))
    closing_index <- which(bracket_balance == 0)[1]
    iterate(
      c(
        as.character(iterate(
          c(
            substr(first, 1, nchar(first) - 1),
            if (closing_index > 2) tokens[2:(closing_index - 1)],
            substring(tokens[closing_index], 2)
          ),
          verbose)),
        tokens[-(1:closing_index)]
      ),
      verbose
    )
  }
}
parse1 <- function(tokens, verbose = FALSE) {
  if (verbose) cat("starting", sep = "\n")
  iterate(rev(tokens), verbose)
}
test_res1 <- c(71, 51, 26, 437, 12240, 13632)
stopifnot(all(vapply(tests_tokens, parse1, numeric(1)) == test_res1))
format(sum(vapply(x, parse1, numeric(1))), scientific = FALSE) # part one: 8929569623593
test_res2 <- c(231, 51, 46, 1445, 669060, 23340)
