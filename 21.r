x <- readLines("21.txt")
trimmed <- substr(x, 1, nchar(x) - 1) # remove final bracket
splt <- strsplit(trimmed, " ", fixed = TRUE)
brks <- vapply(splt, function(words) match("(contains", words), integer(1))
ingredients <- Map(
  function(words, brk) words[1:(brk - 1)],
  splt,
  brks
)
known_allergens <- Map(
  function(words, brk) {
    gsub(",", "", words[(brk + 1):length(words)], fixed = TRUE)
  },
  splt,
  brks
)
all_ingredients <- sort(unique(Reduce(c, ingredients)))
all_allergens <- sort(unique(Reduce(c, known_allergens)))
dummy <- function(x) {
  array(
    x,
    dim = c(length(all_ingredients), length(all_allergens)),
    dimnames = list(all_ingredients, all_allergens)
  )
}
known_copresent <- simplify2array(Map(
  function(n) {
    `[<-`(dummy(FALSE), ingredients[[n]], known_allergens[[n]], TRUE)
  },
  seq_along(x)
))

# part one
# not(ingredient contains allergen) = (allergen listed, ingredient isn't)
# so for each food, can_contain is FALSE if [ing, present_all] is FALSE

can_contain <- Reduce(
  `&`,
  Map(
    function(n) {
      `[<-`(
        dummy(TRUE),
        TRUE,
        known_allergens[[n]],
        known_copresent[, known_allergens[[n]], n]
      )
    },
    seq_along(x)
  )
)
allergen_free <- rowSums(can_contain) == 0
ingredient_freqs <- rowSums(apply(known_copresent, c(1, 3), any))
sum(ingredient_freqs[allergen_free]) # part one: 2315

# part two: match allergens to ingredients, sort ingredients by allergen
possible_matches <- can_contain[!allergen_free, ]
pair <- function(match_mat, contents) {
  if (all(!match_mat))
    return(contents)
  n_possible <- rowSums(match_mat)
  if (!any(n_possible == 1))
    stop("no unambiguous matches")
  single_match <- rownames(match_mat)[n_possible == 1]
  pairs <- apply(
    match_mat[single_match, , drop = FALSE],
    1,
    function(bools) colnames(match_mat)[bools]
  )
  new_contents <- `[<-`(contents, single_match, pairs)
  new_match_mat <- `[<-`(match_mat, TRUE, pairs, FALSE)
  pair(new_match_mat, new_contents)
}
pair_matches <- function(match_mat) {
  contents <- setNames(character(nrow(match_mat)), rownames(match_mat))
  pair(match_mat, contents)
}
pairings <- pair_matches(possible_matches)
# part two: cfzdnz,htxsjf,ttbrlvd,bbbl,lmds,cbmjz,cmbcm,dvnbh
paste(names(sort(pairings)), collapse = ",")
