# Run once. Not sourced by run.R, so a reproduction does not silently install
# packages into someone else's library.

install.packages(c(
  "xts", "zoo", "lubridate",
  "car", "moments", "tseries",
  "rugarch", "PerformanceAnalytics"
))
