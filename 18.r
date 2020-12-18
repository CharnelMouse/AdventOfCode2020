x <- lapply(
  strsplit(readLines("18.txt"), "", fixed = TRUE),
  function(chars) chars[chars != " "]
)

# Implement some recursion to evaluate bracketed
# expressions first, then determine what to do with flattened expressions.
flatten <- function(tokens, flat_parser) {
  openers <- tokens == "("
  if (all(!openers)) return(flat_parser(tokens))
  start <- which(openers)[1]
  closers <- tokens == ")"
  bracket_balance <- cumsum(openers - closers)
  end <- start + which(bracket_balance[-(1:start)] < bracket_balance[start])[1]
  sub_tokens <- tokens[(start + 1):(end - 1)]
  new_tokens <- c(
    tokens[-(start:length(tokens))],
    as.character(flatten(sub_tokens, flat_parser)),
    tokens[-(1:end)]
  )
  flatten(new_tokens, flat_parser)
}
parse_tokens <- function(tokens, flat_parser) flatten(tokens, flat_parser)

# part one: work from left to right once flattened
flat_parse1 <- function(tokens) {
  int1 <- suppressWarnings(as.numeric(tokens[1]))
  if (length(tokens) == 1) return(int1)
  op <- get(tokens[2])
  int2 <- suppressWarnings(as.numeric(tokens[3]))
  new_int <- op(int1, int2)
  new_tokens <- c(as.character(new_int), tokens[-(1:3)])
  flat_parse1(new_tokens)
}
parse1 <- function(tokens) parse_tokens(tokens, flat_parse1)
format(sum(vapply(x, parse1, numeric(1))), scientific = FALSE)
# 8929569623593

# part two: evaluate + before *
# operators have simple number on either side after flattening
# no need to un-reverse token order once flattened,
# since order of flattened expressions doesn't matter
flat_parse2 <- function(tokens) {
  if (length(tokens) == 1) return(suppressWarnings(as.numeric(tokens)))
  pluses <- tokens == "+"
  if (all(!pluses)) return(eval(parse(text = paste(tokens, collapse = " "))))
  plus_index <- which(pluses)[1]
  new_tokens <- c(
    tokens[-((plus_index - 1):length(tokens))],
    as.character(sum(as.numeric(tokens[c(plus_index - 1, plus_index + 1)]))),
    tokens[-(1:(plus_index + 1))]
  )
  flat_parse2(new_tokens)
}
parse2 <- function(tokens) parse_tokens(tokens, flat_parse2)
format(sum(vapply(x, parse2, numeric(1))), scientific = FALSE)
# 231235959382961
