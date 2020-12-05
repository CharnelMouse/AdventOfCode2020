x <- readLines("2.txt")

# Part one
# Format: XX-XX Y: letters, where XX-XX indicates range for number of times
# letter Y can appear in letters

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
sum(letter_counts >= mins & letter_counts <= maxs)
# 628

# Part two

# Numbers now give two positions in the password
# Exactly one must contain the given letter

checked_letters <- t(mapply(
  function(str, index1, index2) {
    c(
      substr(str, index1, index1),
      substr(str, index2, index2)
    )
  },
  passwords,
  mins,
  maxs
))
sum(rowSums(checked_letters == letters) == 1)
# 705

# All together

x <- readLines("2.txt")
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
sum(letter_counts >= mins & letter_counts <= maxs) # part one
checked_letters <- t(mapply(
  function(str, index1, index2) {
    c(
      substr(str, index1, index1),
      substr(str, index2, index2)
    )
  },
  passwords, mins, maxs
))
sum(rowSums(checked_letters == letters) == 1) # part two
