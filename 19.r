x <- readLines("19.txt")
br <- match("", x)
rules <- x[1:(br - 1)]
messages <- x[(br + 1):length(x)]
sort_rules <- function(rules) {
  split_rules <- strsplit(rules, ": ", fixed = TRUE)
  rules_n <- vapply(split_rules, function(strs) strtoi(strs[1]), integer(1))
  rules_order <- order(rules_n)
  vapply(split_rules, `[`, character(1), 2)[rules_order]
}
sorted_rules <- sort_rules(rules)

check_along_references <- function(str, rule_components, rules) {
  reference_indices <- strtoi(rule_components)
  Reduce(
    function(str, index) {
      if (all(str == ""))
        return(character())
      rem <- checkacc(str, index, rules)
      rem[!is.na(rem)]
    },
    reference_indices,
    init = str
  )
}
checkacc <- function(str, index, rules) {
  if (all(nchar(str) == 0))
    return(NA)
  rule <- rules[index + 1]
  if (rule == "\"a\"") {
    rem <- ifelse(substr(str, 1, 1) == "a", substring(str, 2), NA)
    return(rem[!is.na(rem)])
  }
  if (rule == "\"b\"") {
    rem <- ifelse(substr(str, 1, 1) == "b", substring(str, 2), NA)
    return(rem[!is.na(rem)])
  }
  rule_components <- strsplit(rule, " ", fixed = TRUE)[[1]]
  mid <- which(rule_components == "|")
  if (length(mid) == 0) {
    check_along_references(str, rule_components, rules)
  }else{
    starts <- c(1, mid + 1)
    ends <- c(mid - 1, length(rule_components))
    rem <- unlist(
      Map(
        function(s, e) check_along_references(str, rule_components[s:e], rules),
        starts,
        ends
      )
    )
    rem[!is.na(rem)]
  }
}
check <- function(str, rules) {
  any(checkacc(str, 0, rules) == "")
}

possible <- vapply(messages, check, logical(1), sorted_rules)
sum(possible) # part one: 144

max_len <- max(nchar(messages))
rule_8_replacement <- paste(
  vapply(
    1:max_len,
    function(n) paste(rep("42", n), collapse = " "),
    character(1)
  ),
  collapse = " | "
)
rule_11_replacement <- paste(
  vapply(
    1:max_len,
    function(n) paste(c(rep("42", n), rep("31", n)), collapse = " "),
    character(1)
  ),
  collapse = " | "
)
corrected_rules <- `[<-`(
  sorted_rules,
  c(9, 12),
  c(rule_8_replacement, rule_11_replacement)
)
possible2 <- vapply(messages, check, logical(1), corrected_rules)
sum(possible2) # part two: 260
