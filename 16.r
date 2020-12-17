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
n_rules <- nrow(rules_df)
n_fields <- ncol(other_df)

## create main solution array
rule_match <- function(ticket, rules) {
  # field x rule
  apply(rules[, -1], 1, findInterval, x = ticket)
}
# field x rule x ticket
# main solution array, showing when a ticket's field passes a rule
rule_matches <- vapply(
  1:nrow(other_df),
  function(n) rule_match(other_df[n, ], rules_df),
  matrix(0L, nrow = n_fields, ncol = n_rules)
)

## part one
invalid <- function(rule_matches) {
  # ticket x field
  apply(rule_matches ,c(3, 1), function(ints) all(is.element(ints, c(0, 2, 4))))
}
invalid_entries <- invalid(rule_matches)
sum(other_df[invalid_entries]) # 29851

## part two
agreements <- function(rule_matches) {
  # rule x field x ticket
  apply(
    rule_matches,
    c(1, 3),
    function(ints) is.element(ints, c(1, 3))
  )
}
possible_rule_positions <- function(rule_matches) {
  # rule x field
  apply(agreements(rule_matches), 1:2, all)
}
rp_acc <- function(possible, matches) {
  if (all(!is.na(matches)))
    return(matches)
  single_matches <- colSums(possible) == 1
  stopifnot(any(single_matches))
  new_ints <- apply(possible[, single_matches, drop = FALSE], 2, which)
  new_matches <- `[<-`(
    matches,
    single_matches,
    new_ints
  )
  new_possible <- `[<-`(possible, new_ints, TRUE, FALSE)
  rp_acc(new_possible, new_matches)
}
rule_positions <- function(possible) {
  rp_acc(possible, rep(NA_integer_, nrow(possible)))
}
valid_ticket_rule_matches <- rule_matches[, , apply(!invalid_entries, 1, all)]
possible_positions <- possible_rule_positions(valid_ticket_rule_matches)
positions <- rule_positions(possible_positions)
departure_rule_locations <- match(
  which(startsWith(rules_df[, 1], "departure")),
  positions
)
own_entries <- as.integer(unlist(strsplit(own, ",", fixed = TRUE)))
res <- prod(own_entries[departure_rule_locations])
format(res, scientific = FALSE) # part two: 3029180675981
