# Reproduces the analysis end to end. Run from the repository root.

source("code/data.R")
source("code/descriptive.R")
source("code/garch.R")
source("code/var.R")

P     <- 0.01    # VaR probability level
VALUE <- 1       # portfolio value, so VaR reads as a fraction
WE    <- 3132    # estimation window
TN    <- 4000    # observations retained

# ---- data -------------------------------------------------------------------
d       <- prep()
chroma  <- d$df
returns <- tail(chroma$log_returns, TN)
dates   <- ymd(tail(chroma$Dates, TN))

plot(chroma$Dates, chroma$price, type = "l", las = 1,
     main = "Chroma Case price", xlab = "Date", ylab = "Price (INR)")

# ---- descriptive ------------------------------------------------------------
print(describe(returns))
print(normality(returns))
qq_panels(returns)

# ---- conditional volatility -------------------------------------------------
fits <- fit_all(returns)
print(fits$garch@fit$matcoef)
print(fits$aparch@fit$matcoef)
print(fits$figarch@fit$matcoef)
print(lr_tests(fits))

# ---- Value-at-Risk ----------------------------------------------------------
V <- var_hs(returns, dates, p = P, WE = WE, value = VALUE)

# Refitting on every window is slow: roughly two minutes for APARCH and five
# for FIGARCH per few hundred observations. stop_at caps the loop so the run
# finishes; the project used observations up to 3500.
STOP <- 3500

aparch_spec <- ugarchspec(
  variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE)
)
figarch_spec <- ugarchspec(
  variance.model     = list(model = "fiGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)

V$APARCH  <- var_parametric(returns, dates, aparch_spec,  P, WE, VALUE, STOP)
V$FIGARCH <- var_parametric(returns, dates, figarch_spec, P, WE, VALUE, STOP)

plot(V$date, V$y, type = "l", las = 1, ylab = "Returns and 1-day VaR",
     xlim = as.Date(c("2025-08-01", "2025-12-10")))
lines(V$date, -V$HS,      type = "s", col = "red",   lwd = 2)
lines(V$date, -V$APARCH,  type = "s", col = "green", lwd = 2)
lines(V$date, -V$FIGARCH, type = "s", col = "blue",  lwd = 2)
legend("topleft", legend = c("HS", "APARCH", "FIGARCH"),
       col = c("red", "green", "blue"), lty = 1, lwd = 2, bty = "n")

# ---- backtest ---------------------------------------------------------------
tested <- violations(V[(WE + 1):STOP, ])
print(colSums(tested[, c("HS", "APARCH", "FIGARCH")]))
print(coverage_table(tested, p = P))
