times <- microbenchmark::microbenchmark(
  source("01-short.r"),
  source("01-quick.r"),
  source("02.r"),
  source("03.r"),
  source("04.r"),
  source("05.r"),
  source("06.r"),
  source("07-short.r"),
  source("07-quick.r"),
  source("08.r"),
  source("09.r"),
  source("10.r"),
  source("11.r"),
  source("12.r"),
  source("13.r"),
  source("14.r"),
  source("15.r"),
  source("16.r"),
  source("17.r"),
  source("18.r"),
  source("19.r"),
  source("20.r"),
  source("21.r"),
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
