x <- readLines("16.txt")
breaks <- which(x == "")
rules <- x[seq.int(breaks[1] - 1)]
own <- x[breaks[1] + 2]
other <- x[(breaks[2] + 2):length(x)]
# using Reduce here, since self-goal is to not use regex
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
# adjust rule numbers to allow use of findInterval
rules_df[, c(3, 5)] <- rules_df[, c(3, 5)] + 1L
other_df <- data.frame(
  do.call(
    rbind,
    lapply(strsplit(other, ",", fixed = TRUE), as.integer)
  )
)

rule_match <- function(ticket, rules) {
  # field x rule
  apply(rules[, -1], 1, findInterval, x = ticket)
}
invalid_field <- function(ticket, rules) {
  intervals <- rule_match(ticket, rules)
  # field
  apply(intervals, 1, function(ints) all(is.element(ints, c(0, 2, 4))))
}
invalid_entries <- t(apply(other_df, 1, invalid_field, rules_df))
sum(other_df[invalid_entries]) # part one: 29851

agreements <- function(ticket, rules) {
  intervals <- rule_match(ticket, rules)
  # field x rule
  apply(intervals, 2, function(ints) is.element(ints, c(1, 3)))
}
possible_rule_matches <- function(tickets, rules) {
  # field x rule
  Reduce(
    function(ls, n) {
      p <- agreements(tickets[n, ], rules)
      ls & p
    },
    1:nrow(tickets),
    init = match(TRUE, ncol(tickets), nrow(rules))
  )
}
rp_acc <- function(possible, matches) {
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
  rp_acc(new_possible, new_matches)
}
rule_positions <- function(possible) {
  rp_acc(possible, rep(NA_integer_, ncol(possible)))
}
valid_tickets <- other_df[!apply(invalid_entries, 1, any), , drop = FALSE]
final_possible <- possible_rule_matches(valid_tickets, rules_df)
stopifnot(any(rowSums(final_possible) == 1))
rule_locations <- rule_positions(final_possible)
departure_rule_locations <- match(
  which(startsWith(rules_df[, 1], "departure")),
  rule_locations
)
own_entries <- as.integer(unlist(strsplit(own, ",", fixed = TRUE)))
res <- prod(own_entries[departure_rule_locations])
format(res, scientific = FALSE) # part two: 3029180675981
