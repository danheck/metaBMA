library("rstan")

STAN_CONTROL <- list(adapt_delta = .9)

set.seed(12352)
SE <- runif(20, .3,1.3)
d <- data.frame(yyy = rnorm(20, 0, SE), SE = SE, study = 1:20)
d$xx <- rnorm(20, 0, 1)
d$cat <- rep(c("a", "b"), 10)


test_that("bma works for fitted meta_* objects", {
  f1 <- meta_fixed(yyy, SE, study, data = d, control = STAN_CONTROL)
  f1a <- meta_fixed(yyy ~ 1, SE, study, data = d, logml = "stan", summarize = "stan",
                    iter = 6000, warmup = 1000, control = STAN_CONTROL)
  expect_silent(bb <- bma(list(a = f1, b = f1a)))
  expect_equal(bb$posterior_models, c(a = .5, b = .5), tolerance = .01)

  r1 <- meta_random(yyy, SE, study, data = d, control = STAN_CONTROL)
  r1a <- meta_random(yyy ~ 1, SE, study, data = d, logml = "stan", summarize = "stan",
                    iter = 6000, warmup = 1000, control = STAN_CONTROL)
  expect_silent(bb <- bma(list(a = r1, b = r1a)))
  expect_equal(bb$posterior_models, c(a = .5, b = .5), tolerance = .01)

  expect_silent(bb <- bma(list(a = f1, b = f1a, c= r1, d = r1a)))
  mean_avg <- sum(bb$posterior_models * bb$estimates[-1,"mean"])
  expect_equal(mean_avg, bb$estimates["Averaged","mean"], tolerance = .005)
})

test_that("meta_bma gives identical results for stan/integrate", {

  mf_stan <- meta_bma(yyy, SE, study, d, summarize = "stan", logml = "stan",
                                    control = STAN_CONTROL, iter = 7000, warmup = 1000)
  mf_int <- meta_bma(yyy, SE, study, d, summarize = "int", control = STAN_CONTROL)
  expect_equal(mf_stan$estimates, mf_int$estimates, tolerance = .03)

  expect_silent(plot_forest(mf_stan))
  expect_silent(plot_forest(mf_int))
  expect_silent(plot_posterior(mf_int, from=0, to=1))
  expect_silent(plot_posterior(mf_stan))
})


