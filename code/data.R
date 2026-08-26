# Load the Chroma Case price history and build a daily log-return series.

library(xts)

prep <- function(path = "data/chroma_case_pricehistory.csv") {
  x <- read.csv(path, stringsAsFactors = FALSE)
  x$timestamp_iso <- NULL

  x$log_returns <- c(NA, diff(log(x$price)))
  x <- x[-1, ]
  names(x)[1] <- "Dates"

  # timestamps arrive as "Jan 09 2015 01: +0": strip the timezone fragment,
  # then repair the truncated hour before parsing
  d <- sub("\\s*\\+.*$", "", x$Dates)
  d <- sub("([0-9]{2}):$", "\\1:00", d)
  x$Dates <- as.Date(as.POSIXct(d, format = "%b %d %Y %H:%M", tz = "UTC"))

  list(df = x, ts = xts(x$log_returns, order.by = x$Dates))
}
