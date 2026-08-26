# Unconditional moments, tail behaviour and a normality test.

library(moments)
library(tseries)
library(car)

describe <- function(r) {
  data.frame(
    mean     = mean(r),
    sd       = sd(r),
    min      = min(r),
    max      = max(r),
    skewness = skewness(r),
    kurtosis = kurtosis(r)
  )
}

# Returns are compared against Student-t with 3 and 2.5 degrees of freedom as
# well as the normal. The t comparisons are the informative ones: the normal
# plot exists to show how badly it fails.
qq_panels <- function(r) {
  qqPlot(r, distribution = "t", df = 3,   envelope = FALSE,
         main = "Chroma returns vs Student-t(3)",
         xlab = "Theoretical quantiles", ylab = "Sample quantiles")
  qqPlot(r, distribution = "t", df = 2.5, envelope = FALSE,
         main = "Chroma returns vs Student-t(2.5)",
         xlab = "Theoretical quantiles", ylab = "Sample quantiles")
  qqPlot(r, envelope = FALSE,
         main = "Chroma returns vs normal",
         xlab = "Theoretical quantiles", ylab = "Sample quantiles")
}

normality <- function(r) {
  list(
    test     = jarque.bera.test(r),
    critical = qchisq(0.99, df = 2)
  )
}
