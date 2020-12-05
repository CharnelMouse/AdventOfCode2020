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
      all_chars_in(pid, 0:9)
    )
  })
}
length(passports_with_valid_fields) # part one 219
sum(vapply(passports_with_valid_fields, valid_values, logical(1))) # part two 127
