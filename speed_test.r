times <- microbenchmark::microbenchmark(
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
  source("10.r"),
  source("11.r"),
  source("12.r"),
  source("13.r"),
  source("14.r"),
  source("15.r"),
  source("16.r"),
  source("17.r"),
  setup = expression(rm(list = ls()))
)

time_summary <- function(times, scale = 1) {
  data.table::as.data.table(times)[
    ,
    .(
      mean = mean(time/scale),
      sd = sd(time/scale),
      se = sd(time/scale)/.N,
      `low (2.5%)` = quantile(time/scale, 0.025),
      `high (97.5%)` = quantile(time/scale, 0.975)
    ),
    by = expr
    ]
}
