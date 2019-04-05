library(rstan)
library(testthat)
library(metaBMA)

set.seed(12352)
SE <- runif(20, .3, 1.3)
dat <- data.frame(yyy = rnorm(20, 0, SE), SE = SE, study = 1:20)
dat$xx <- rnorm(20, 0, 1)
dat$cat <- rep(c("a", "b"), 10)


test_that("bma works for fitted meta_* objects", {
  f1a <- meta_fixed(yyy, SE, study, data = dat, chains = 1)
  f1b <- meta_fixed(yyy ~ 1, SE, study, data = dat, logml = "stan", summarize = "stan")
  expect_silent(bb <- bma(list(a = f1a, b = f1b)))
  postprob <- unname(bb$posterior_models[c(2,4)])
  expect_equal(postprob/sum(postprob), c(.5, .5), tolerance = .01)

  expect_is(f1a$logml, "numeric")
  expect_is(f1a$BF, "matrix")
  expect_is(f1a$estimates, "matrix")

  r1a <- meta_random(yyy, SE, study, data = dat)
  r1b <- meta_random(yyy ~ 1, SE, study, data = dat, logml = "stan", summarize = "stan")
  expect_silent(bb <- bma(list(a = r1a, b = r1b)))
  postprob <- unname(bb$posterior_models[c(2,4)])
  expect_equal(postprob/sum(postprob), c(.5, .5), tolerance = .01)

  expect_is(r1a$logml, "numeric")
  expect_is(r1a$BF, "matrix")
  expect_is(r1a$estimates, "matrix")

  expect_silent(bb <- bma(list(a = f1a, b = f1b, c= r1a, d = r1b)))
  mean_avg <- sum(bb$posterior_models * bb$estimates[-1,"mean"])
  expect_equal(mean_avg, bb$estimates["averaged","mean"], tolerance = .005)

  expect_is(bb$logml, "numeric")
  expect_is(bb$BF, "matrix")
  expect_is(bb$estimates, "matrix")
})

test_that("meta_bma gives identical results for stan/integrate", {

  mf_stan <- meta_bma(yyy, SE, study, dat, summarize = "stan", logml = "stan")
  mf_int <- meta_bma(yyy, SE, study, dat, summarize = "int")
  expect_equal(mf_stan$estimates, mf_int$estimates, tolerance = .03)

  expect_silent(plot_forest(mf_stan))
  expect_silent(plot_forest(mf_int))
  expect_silent(plot_posterior(mf_int, from=0, to=1))
  expect_silent(plot_posterior(mf_stan))
})


