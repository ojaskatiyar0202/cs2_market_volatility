# One-day Value-at-Risk by historical simulation and by GARCH-family forecast,
# then Bernoulli coverage backtesting of the violation sequence.

library(rugarch)
library(lubridate)

# Historical simulation: VaR is just the p-th order statistic of the estimation
# window. No distributional assumption, but it cannot react faster than the
# window rolls.
var_hs <- function(returns, dates, p = 0.01, WE = 3132, value = 1) {
  T <- length(returns)
  out <- data.frame(date = dates, y = returns, HS = NA_real_)
  for (t in (WE + 1):T) {
    window <- returns[(t - WE):(t - 1)]
    out$HS[t] <- -sort(window)[floor(WE * p)] * value
  }
  out
}

# Parametric VaR: refit the model on each rolling window, forecast the one-step
# conditional variance, then take the p-quantile of the assumed distribution.
#
# The forecast is built with ugarchforecast rather than by indexing coef(fit),
# because coefficient ordering differs across models. APARCH carries gamma and
# delta and FIGARCH carries d, so assuming positions 1 to 3 are omega, alpha and
# beta silently produces the wrong variance for both.
var_parametric <- function(returns, dates, spec, p = 0.01, WE = 3132,
                           value = 1, stop_at = NULL, verbose = TRUE) {
  T <- if (is.null(stop_at)) length(returns) else stop_at
  out <- rep(NA_real_, length(returns))

  for (t in (WE + 1):T) {
    if (verbose) cat(t, "")
    window <- returns[(t - WE):(t - 1)]
    fit <- tryCatch(
      ugarchfit(spec = spec, data = window, solver = "solnp"),
      error = function(e) NULL
    )
    if (is.null(fit)) next

    fc <- ugarchforecast(fit, n.ahead = 1)
    sigma1 <- as.numeric(sigma(fc))
    out[t] <- -value * qnorm(p, sd = sigma1)
  }
  if (verbose) cat("\n")
  out
}

# A violation is a day where the realised return breached the forecast.
violations <- function(v, methods = c("HS", "APARCH", "FIGARCH")) {
  for (m in methods) {
    v[, m] <- as.integer(v$y < -v[, m])
  }
  v
}

# Kupiec unconditional coverage test. Under the null the violation rate equals
# p, so the observed rate should not differ from p by more than sampling noise.
bern_test <- function(p, v) {
  lv <- length(v)
  sv <- sum(v)
  if (sv == 0) return(NA_real_)
  al <- log(p) * sv + log(1 - p) * (lv - sv)          # null: rate is p
  bl <- log(sv / lv) * sv + log(1 - sv / lv) * (lv - sv)  # alternative: observed
  -2 * (al - bl)
}

coverage_table <- function(v, p = 0.01, methods = c("HS", "APARCH", "FIGARCH")) {
  critical <- qchisq(0.95, df = 1)
  do.call(rbind, lapply(methods, function(m) {
    stat <- bern_test(p, v[, m])
    data.frame(
      method         = m,
      violation_rate = mean(v[, m]),
      expected_rate  = p,
      lr_stat        = stat,
      p_value        = 1 - pchisq(stat, df = 1),
      reject_at_5pct = !is.na(stat) && stat > critical
    )
  }))
}
