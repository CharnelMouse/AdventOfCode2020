x <- strsplit(readLines("14.txt"), " = ", fixed = TRUE)
cmds <- vapply(x, `[`, character(1), 1)
vals <- vapply(x, `[`, character(1), 2)
mask_breaks <- cmds == "mask"
# work backwards, so we can remove assignments at addresses that are changed later
mem_cmds <- rev(cmds[!mask_breaks])
mem_addresses <- as.numeric(substring(mem_cmds, 5, nchar(mem_cmds) - 1))
mem_vals <- rev(as.numeric(vals[!mask_breaks]))
mem_block_lengths <- rev(diff(c(which(mask_breaks), length(cmds) + 1L)) - 1L)
mem_masks <- rep(rev(vals[mask_breaks]), mem_block_lengths)
# part one
# remove assigments at addresses changed again later
dup_address <- duplicated(mem_addresses)
used_addresses <- mem_addresses[!dup_address]
used_vals <- mem_vals[!dup_address]
used_masks <- mem_masks[!dup_address]
# no numeric -> binary converter, just from 32-bit int (to bits), so we write our own
# numbers on rows
numToBin <- function(num, length) {
  vapply(
    (length - 1):0,
    function(n) (num %% 2^(n + 1)) %/% 2^n,
    numeric(length(num))
  )
}
mask_val <- function(vals, masks) {
  if (length(vals) != length(masks))
    stop("inputs must be equal length")
  len <- nchar(masks[1])
  valBins <- numToBin(vals, len)
  maskBins <- do.call(rbind, strsplit(masks, "", fixed = TRUE))
  finalBins <- ifelse(maskBins == "X", valBins, suppressWarnings(as.numeric(maskBins)))
  colSums(2^((len - 1):0) * t(finalBins))
}
format(sum(mask_val(used_vals, used_masks)), scientific = FALSE) # part one: 10050490168421
# part two
# mask addresses before checking for duplicates
mask_single_address_bin <- function(addBin, maskBin)
mask_address <- function(addresses, masks) {
  if (length(addresses) != length(masks))
    stop("inputs must be equal length")
  len <- nchar(masks[1])
  addBins <- numToBin(addresses, len)
  maskBins <- do.call(rbind, strsplit(masks, "", fixed = TRUE))
  finalBins <- ifelse(maskBins == "X", addBins, suppressWarnings(as.numeric(maskBins)))
  colSums(2^((len - 1):0) * t(finalBins))
}


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
