x <- strsplit(readLines("14.txt"), " = ", fixed = TRUE)
cmds <- vapply(x, `[`, character(1), 1)
vals <- vapply(x, `[`, character(1), 2)
mask_breaks <- cmds == "mask"
# work backwards, so we can remove assignments at addresses that are changed later
mem_cmds <- rev(cmds[!mask_breaks])
mem_addresses <- as.numeric(substring(mem_cmds, 5, nchar(mem_cmds) - 1))
mem_vals <- rev(as.numeric(vals[!mask_breaks]))
mem_block_lengths <- rev(diff(c(which(mask_breaks), length(cmds) + 1L)) - 1L)
mem_masks <- rev(vals[mask_breaks])
blocked_masks <- rep(mem_masks, mem_block_lengths)
# part one
# remove assigments at addresses changed again later
dup_address <- duplicated(mem_addresses)
used_addresses <- mem_addresses[!dup_address]
used_vals <- mem_vals[!dup_address]
used_masks <- blocked_masks[!dup_address]
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
n_float <- function(masks) {
  maskBins <- do.call(rbind, strsplit(masks, "", fixed = TRUE))
  rowSums(maskBins == "X")
}
mask_address <- function(addresses, masks) {
  if (length(addresses) != length(masks))
    stop("inputs must be equal length")
  len <- nchar(masks[1])
  maskBins <- do.call(rbind, strsplit(masks, "", fixed = TRUE))
  n_floats <- rowSums(maskBins == "X")
  blocked_adds <- rep(addresses, 2^n_floats)
  # masks on columns
  blocked_maskBins <- t(maskBins[rep(seq.int(length(masks)), 2^n_floats), ])
  blocked_maskBins[blocked_maskBins == "0"] <- NA_character_
  float_values <- do.call(
    c,
    lapply(n_floats, function(n) t(expand.grid(rep(list(0:1), n))))
  )
  blocked_maskBins[!is.na(blocked_maskBins) & blocked_maskBins == "X"] <- float_values
  # addresses on columns
  blocked_addBins <- t(numToBin(blocked_adds, len))
  masked_addBins <- blocked_addBins
  mask_has_effects <- !is.na(blocked_maskBins)
  masked_addBins[mask_has_effects] <- as.numeric(blocked_maskBins[mask_has_effects])
  dim(masked_addBins) <- c(len, length(masked_addBins)/len) # only useful for length 1
  colSums(2^((len - 1):0) * masked_addBins)
}
masked_addresses <- mask_address(mem_addresses, blocked_masks)
used_vals2 <- rep(mem_vals, 2^n_float(blocked_masks))[!duplicated(masked_addresses)]
format(sum(used_vals2), scientific = FALSE) # part two: 2173858456958
