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

complete <- function(intchars, rules) {
  sections <- lapply(strtoi(intchars), function(n) rules[[n + 1]])
  Reduce(
    function(x, y) {
      as.character(outer(x, y, paste, sep = ""))
    },
    sections
  )
}

iterate <- function(rules, dependencies, completed) {
  if (isTRUE(completed[1]))
    return(rules[[1]])
  completeable <- vapply(
    dependencies,
    function(ints) all(completed[ints + 1]),
    logical(1)
  )
  todo <- completeable & !completed
  res <- lapply(
    strsplit(as.character(rules)[todo], " ", fixed = TRUE),
    function(chars) {
      mid <- match("|", chars)
      if (is.na(mid)) {
        complete(chars, rules)
      }else
        c(
          complete(chars[1:(mid - 1)], rules),
          complete(chars[(mid + 1):length(chars)], rules)
        )
    }
  )
  new_rules <- `[<-`(rules, todo, res)
  iterate(new_rules, dependencies, completed | todo)
}
resolve <- function(rules) {
  tidy_rules <- gsub("\"", "", rules, fixed = TRUE)
  completed <- is.element(tidy_rules, c("a", "b"))
  dependencies <- lapply(
    strsplit(tidy_rules, " ", fixed = TRUE),
    function(chars) strtoi(
      setdiff(
        chars,
        c("|", "a", "b")
      )
    )
  )
  iterate(as.list(tidy_rules), dependencies, completed)
}
possible <- resolve(sorted_rules)
sum(is.element(messages, possible)) # part one: 144
