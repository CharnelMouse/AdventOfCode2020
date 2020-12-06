x <- do.call(cbind, strsplit(readLines("5.txt"), ""))
bin <- matrix(as.integer(x == "B" | x == "R"), nrow = nrow(x))
ids <- colSums(bin * 2^((nrow(x) - 1):0))
max(ids) # part one
present <- rle(is.element(0:(2^nrow(x) - 1), ids))
index <- 1L + which(present$lengths[-1] == 1 & present$values[-1] == FALSE)[1]
sum(present$lengths[1:index]) - 1 # part two
