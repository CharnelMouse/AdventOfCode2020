x <- readLines("02.txt")
parts <- strsplit(x, "[: -]+")
mins <- as.integer(vapply(parts, `[`, character(1), 1))
maxs <- as.integer(vapply(parts, `[`, character(1), 2))
letters <- vapply(parts, `[`, character(1), 3)
passwords <- vapply(parts, `[`, character(1), 4)
letter_counts <- mapply(
  function(chars, char) sum(chars == char),
  chars = strsplit(passwords, ""),
  char = letters
)
sum(letter_counts >= mins & letter_counts <= maxs) # part one: 628
checked_letters <- t(mapply(
  function(str, index1, index2) {
    c(
      substr(str, index1, index1),
      substr(str, index2, index2)
    )
  },
  passwords, mins, maxs
))
sum(rowSums(checked_letters == letters) == 1) # part two: 705
