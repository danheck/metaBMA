
# set.seed(123)
# y <- rnorm(20, 1,.1)
# se <- runif(20, .1)
library(rjags)
data(towels)

test_that("extreme priors/misspecified models still provide correct results", {
  expect_silent(mf <- meta_bma(towels$logOR, towels$SE, towels$study,
                               d = "cauchy", d.par = c(scale = 1/sqrt(2)),
                               tau = "halfcauchy", tau.par = c(.5)))
  expect_silent(mr <- meta_random(towels$logOR, towels$SE, towels$study,d.par = c(mean = 0.2, sd = .05)))
})
