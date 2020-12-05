x <- paste(readLines("4.txt"), collapse = "\n")
passports <- strsplit(
  gsub("\n", " ", strsplit(x, "\n\n", fixed = TRUE)[[1]], fixed = TRUE),
  " "
)
clean_passports <- lapply(
  passports,
  function(strs) setNames(substring(strs, 5), substr(strs, 1, 3))
)

# part one

required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
has_all_required_fields <- vapply(
  clean_passports,
  function(flds) all(is.element(required_fields, names(flds))),
  logical(1)
)
passports_with_valid_fields <- clean_passports[has_all_required_fields]
length(passports_with_valid_fields) #219

# part two

int_range <- function(txt, min, max) {
  x <- as.integer(txt)
  grepl("^[0-9]+$", txt) && x >= min && x <= max
}
valid_values <- function(passport) {
  with(as.list(passport), {
    hgt_len <- nchar(hgt)
    all(
      grepl("^[0-9]{4}$", byr),
      int_range(byr, 1920, 2002),
      grepl("^[0-9]{4}$", iyr),
      int_range(iyr, 2010, 2020),
      grepl("^[0-9]{4}$", eyr),
      int_range(eyr, 2020, 2030),
      switch(
        substr(hgt, hgt_len - 1, hgt_len),
        cm = int_range(strtrim(hgt, hgt_len - 2), 150, 193),
        `in` = int_range(strtrim(hgt, hgt_len - 2), 59, 76),
        FALSE
      ),
      grepl("^#[0-9a-f]{6}$", hcl),
      is.element(
        ecl,
        c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      ),
      grepl("^[0-9]{9}$", pid)
    )
  })
}
sum(vapply(passports_with_valid_fields, valid_values, logical(1))) # 127

# Neat version

x <- paste(readLines("4.txt"), collapse = "\n")
passports <- strsplit(
  gsub(
    "\n", " ",
    unlist(strsplit(x, "\n\n", fixed = TRUE)),
    fixed = TRUE
  ),
  " "
)
clean_passports <- lapply(
  passports,
  function(strs) setNames(substring(strs, 5), substr(strs, 1, 3))
)
required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
passports_with_valid_fields <- clean_passports[
  vapply(
    clean_passports,
    function(flds) all(is.element(required_fields, names(flds))),
    logical(1)
  )
  ]
all_chars_in <- function(str, valid) {
  all(is.element(unlist(strsplit(str, "")), valid))
}
int_range <- function(txt, min, max) {
  x <- as.integer(txt)
  all_chars_in(txt, 0:9) && x >= min && x <= max
}
valid_values <- function(passport) {
  with(as.list(passport), {
    hgt_len <- nchar(hgt)
    all(
      nchar(byr) == 4,
      int_range(byr, 1920, 2002),
      nchar(iyr) == 4,
      int_range(iyr, 2010, 2020),
      nchar(eyr) == 4,
      int_range(eyr, 2020, 2030),
      switch(
        substr(hgt, hgt_len - 1, hgt_len),
        cm = int_range(strtrim(hgt, hgt_len - 2), 150, 193),
        `in` = int_range(strtrim(hgt, hgt_len - 2), 59, 76),
        FALSE
      ),
      nchar(hcl) == 7,
      substr(hcl, 1, 1) == "#",
      all_chars_in(substring(hcl, 2), c(0:9, letters[1:6])),
      is.element(
        ecl,
        c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      ),
      nchar(pid) == 9,
      all(is.element(unlist(strsplit(pid, "")), 0:9))
    )
  })
}
length(passports_with_valid_fields) # part one 219
sum(vapply(passports_with_valid_fields, valid_values, logical(1))) # part two 127
