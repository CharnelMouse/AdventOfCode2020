x <- readLines("16.txt")
breaks <- which(x == "")
rules <- x[seq.int(breaks[1] - 1)]
own <- x[breaks[1] + 2]
other <- x[(breaks[2] + 2):length(x)]
rules_df <- data.frame(
  do.call(
    rbind,
    Reduce(
      function(strs, split) {
        lapply(
          strs,
          function(str) unlist(strsplit(str, split, fixed = TRUE))
        )
      },
      c(": ", " or ", "-"),
      init = rules
    )
  ),
  stringsAsFactors = FALSE
)
rules_df[, -1] <- vapply(rules_df[, -1], as.integer, integer(nrow(rules_df)))
rules_df[, c(3, 5)] <- rules_df[, c(3, 5)] + 1L
other_df <- data.frame(
  do.call(
    rbind,
    strsplit(other, ",", fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
other_df[] <- vapply(other_df, as.integer, integer(nrow(other_df)))
rule_match <- function(ticket, rules) {
  # field x rule
  apply(rules[, -1], 1, findInterval, x = ticket)
}
invalid <- function(ticket, rules) {
  intervals <- rule_match(ticket, rules)
  # field
  apply(intervals, 1, function(ints) all(is.element(ints, c(0, 2, 4))))
}
invalid_els <- t(apply(other_df, 1, invalid, rules_df))
sum(other_df[invalid_els]) # part one: 29851
invalid_tickets <- apply(invalid_els, 1, any)
valid_df <- other_df[!invalid_tickets, , drop = FALSE]
possible <- function(ticket, rules) {
  intervals <- rule_match(ticket, rules)
  # field x rule
  apply(intervals, 2, function(ints) is.element(ints, c(1, 3)))
}
possible_arr <- function(tickets, rules) {
  # field x rule
  Reduce(
    function(ls, n) {
      p <- possible(tickets[n, ], rules)
      ls & p
    },
    1:nrow(tickets),
    init = match(TRUE, ncol(tickets), nrow(rules))
  )
}
final_possible <- possible_arr(valid_df, rules_df)
stopifnot(any(rowSums(final_possible) == 1))
match_acc <- function(possible, matches) {
  if (all(!is.na(matches)))
    return(matches)
  single_matches <- rowSums(possible) == 1
  new_ints <- apply(possible[single_matches, , drop = FALSE], 1, which)
  new_matches <- `[<-`(
    matches,
    single_matches,
    new_ints
  )
  new_possible <- `[<-`(possible, TRUE, new_ints, FALSE)
  match_acc(new_possible, new_matches)
}
match_fields <- function(possible) {
  match_acc(possible, rep(NA_integer_, ncol(possible)))
}
rule_locations <- match_fields(final_possible)
departure_rule_locations <- match(
  which(startsWith(rules_df[, 1], "departure")),
  rule_locations
)
own_int <- as.integer(unlist(strsplit(own, ",", fixed = TRUE)))
res <- prod(own_int[departure_rule_locations])
format(res, scientific = FALSE)# part two: 3029180675981
