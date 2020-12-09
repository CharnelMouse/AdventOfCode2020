ops <- read.delim(
  "8.txt", header = FALSE, sep = " ", as.is = TRUE, col.names = c("op", "n")
)
n_ops <- nrow(ops) # Pass this in everywhere explictly to avoid recalculating it
exp_parse <- function(op, n, acc, index) {
  switch(
    op,
    acc = c(acc + n, index + 1L),
    jmp = c(acc    , index + n),
    nop = c(acc    , index + 1L)
  )
}
execute <- function(ops, n_ops, index, acc, index_history, acc_history) {
  if (index > n_ops || any(index_history == index))
    return(list(
      acc = acc,
      index_history = index_history,
      acc_history = acc_history
    ))
  res <- exp_parse(ops$op[index], ops$n[index], acc, index)
  if (index == n_ops)
    res[1]
  else
    execute(ops, n_ops, res[2], res[1], c(index_history, index), c(acc_history, acc))
}
start <- function(ops, n_ops) execute(ops, n_ops, 1L, 0L, integer(), integer())
test_swap <- function(ops, n_ops, replace, base) {
  new_ops <- ops
  new_ops$op[replace] <- ifelse(new_ops$op[replace] == "jmp", "nop", "jmp")
  history_len <- match(replace, base$index_history)
  acc <- if (history_len == length(base$acc_history))
    base$acc
  else
    base$acc_history[history_len]
  index_history <- base$index_history[0:(history_len - 1L)]
  acc_history <- base$acc_history[0:(history_len - 1L)]
  execute(new_ops, n_ops, replace, acc, index_history, acc_history)
}
fix <- function(ops, n_ops, base) {
  # change ops later in history first, since expect long history when correct
  swap_indices <- rev(
    base$index_history[
      is.element(ops$op[base$index_history], c("jmp", "nop"))
      ]
  )
  for (n in swap_indices) {
    tst <- test_swap(ops, n_ops, n, base)
    if (is.integer(tst))
      break
  }
  tst
}
base <- start(ops, n_ops)
base$acc # part one: 1317
fix(ops, n_ops, base) # part two: 1033
