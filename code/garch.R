# Fit GARCH(1,1), APARCH(1,1) and FIGARCH(1,1), then compare by likelihood
# ratio against the GARCH baseline.
#
# APARCH adds a leverage term and a power parameter, so it nests GARCH with two
# extra parameters. FIGARCH adds a fractional differencing parameter d, which is
# what allows shocks to decay hyperbolically rather than geometrically: the
# long-memory property this project is testing for.

library(rugarch)

fit_all <- function(r) {
  garch <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE)
  )
  aparch <- ugarchspec(
    variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE)
  )
  figarch <- ugarchspec(
    variance.model     = list(model = "fiGARCH", garchOrder = c(1, 1)),
    mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = "std"
  )

  list(
    garch   = ugarchfit(spec = garch,   data = r, solver = "hybrid"),
    aparch  = ugarchfit(spec = aparch,  data = r, solver = "hybrid"),
    figarch = ugarchfit(spec = figarch, data = r, solver = "hybrid")
  )
}

# Both alternatives nest GARCH, so 2*(L1 - L0) is chi-squared under the null.
# Degrees of freedom differ: APARCH adds gamma and delta, FIGARCH adds d and
# the t shape parameter.
lr_tests <- function(fits) {
  lr <- function(alt) 2 * (likelihood(alt) - likelihood(fits$garch))
  data.frame(
    model     = c("APARCH", "FIGARCH"),
    lr_stat   = c(lr(fits$aparch), lr(fits$figarch)),
    df        = c(2, 2),
    critical  = qchisq(0.99, df = 2)
  )
}
