x <- do.call(rbind, strsplit(readLines("14.txt"), " = ", fixed = TRUE))
mask <- 0L
# Inbuild converter int -> binary is only for 32-bit, so we need to write our own.
singleBit <- function(x, bit) {
  as.integer((x %% 2^(bit + 1)) %/% 2^bit)
}
toBin <- function(x, length) {
  test <- any(x >= 2^length)
  if (is.na(test))
    stop(paste(x, collapse = "\n"))
  if (test)
    stop("too short")
  vapply(x, singleBit, integer(length), (length - 1):0)
}
combine <- function(x, mask, length) {
  bits <- toBin(x, length)
  mask_chars <- unlist(strsplit(mask, ""))
  bits[mask_chars == "0", ] <- 0
  bits[mask_chars == "1", ] <- 1
  colSums(2^((length - 1):0)*bits)
}
acc <- function(cmds, n_cmds, mask_inds, mem, length) {
  if (length(mask_inds) == 0)
    return(mem)
  if (cmds[1, 1] != "mask")
    stop(paste("block doesn't start with mask"))
  block_indices <- 2:(if (length(mask_inds) == 1) n_cmds else mask_inds[2] - 1)
  block_cmds <- cmds[block_indices, 1]
  indices <- substr(block_cmds, 5, nchar(block_cmds) - 1)
  vals <- gmp::as.bigz(cmds[block_indices, 2])
  if (any(is.na(vals)))
    stop(paste(cmds[block_indices, 2][is.na(vals)], collapse = "\n"))
  masked_vals <- combine(vals, cmds[1, 2], length)
  acc(
    cmds[-(c(1, block_indices)), , drop = FALSE],
    n_cmds - block_indices[length(block_indices)],
    mask_inds[-1] - block_indices[length(block_indices)],
    `[<-`(mem, indices, masked_vals),
    length
  )
}
run <- function(cmds, length) {
  acc(cmds, nrow(cmds), which(cmds[, 1] == "mask"), numeric(), length)
}
res <- run(x, 36)
format(sum(res), scientific = FALSE) # part one: 10050490168421
mask_index <- function(index, mask, length) {
  bits <- toBin(index, length)
  mask_chars <- unlist(strsplit(mask, ""))
  bits[mask_chars == "1", ] <- 1
  n_Xs <- sum(mask_chars == "X")
  vapply(
    seq.int(2^n_Xs) - 1L,
    function(n) {
      val <- bits
      val[mask_chars == "X", ] <- toBin(n, n_Xs)
      paste0(val, collapse = "")
    },
    character(1)
  )
}
update_mem <- function(mem, mem_addresses, vals, indices) {
  ind_matches <- vapply(
    indices,
    function(n) {
      match <- which(n == mem_addresses)
      if (length(match) == 0) NA_integer_ else match
    },
    integer(1)
  )
  new <- is.na(ind_matches)
  mem[ind_matches[!new]] <- vals[!new]
  new_mem <- c(mem, vals[new])
  new_mem_addresses <- c(mem_addresses, indices[new])
  if (length(new_mem) != length(mem) + sum(new))
    stop("wat")
  list(new_mem, new_mem_addresses)
}
acc2 <- function(cmds, mask, mem, mem_addresses, length) {
  if (nrow(cmds) == 0)
    return(mem)
  if (cmds[1, 1] == "mask")
    acc2(cmds[-1, , drop = FALSE], cmds[1, 2], mem, mem_addresses, length)
  else{
    address <- substr(cmds[1, 1], 5, nchar(cmds[1, 1]) - 1)
    masked_addresses <- mask_index(
      as.numeric(address),
      mask,
      length
    )
    new_mem <- update_mem(
      mem,
      mem_addresses,
      rep(cmds[1, 2], length(masked_addresses)),
      masked_addresses
    )
    acc2(
      cmds[-1, , drop = FALSE],
      mask,
      new_mem[[1]],
      new_mem[[2]],
      length
    )
  }
}
run2 <- function(cmds, length) {
  acc2(cmds, "", character(), character(), length)
}
res2 <- run2(x, 36)
format(sum(gmp::as.bigz(res2)), scientific = FALSE) # part two: 2173858456958
