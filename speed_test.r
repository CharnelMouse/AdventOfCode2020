microbenchmark::microbenchmark(
  source("1-short.r"),
  source("1-quick.r"),
  source("2.r"),
  source("3.r"),
  source("4.r"),
  source("5.r"),
  source("6.r"),
  source("7-short.r"),
  source("7-quick.r"),
  source("8.r"),
  source("9.r"),
  setup = expression(rm(list = ls()))
)
