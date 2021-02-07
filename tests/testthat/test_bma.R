library(rstan)
library(testthat)
library(metaBMA)

set.seed(12352)
SE <- runif(20, .2, .8)
dat <- data.frame(yyy = rnorm(20, rnorm(20, .2, .3), SE), SE = SE, study = 1:20)
dat$xx <- rnorm(20, 0, 1)
dat$cat <- rep(c("a", "b"), 10)


test_that("bma works for fitted meta_* objects", {
  set.seed(12352)
  f1a <- meta_fixed(yyy, SE, study, data = dat, chains = 1, rel.tol = .1)
  f1b <- meta_fixed(yyy ~ 1, SE, study,
    data = dat,
    iter = 2000, logml = "stan", summarize = "stan", rel.tol = .1
  )
  expect_silent(bb <- bma(list("a" = f1a, "b" = f1b)))
  postprob <- unname(bb$posterior_models[c(2, 4)])
  expect_equal(postprob / sum(postprob), c(.5, .5), tolerance = .001)
  expect_equal(f1a$estimates[, 1:7], f1b$estimates[, 1:7], tolerance = .02)

  expect_is(f1a$logml, "numeric")
  expect_is(f1a$BF, "matrix")
  expect_is(f1a$estimates, "matrix")


  suppressWarnings(r1b <- meta_random(yyy ~ 1, SE, study,
    data = dat,
    logml = "stan", logml_iter = 1500, iter = 100
  ))
  expect_is(r1b$logml, "numeric")
  expect_is(r1b$BF, "matrix")
  expect_is(r1b$estimates, "matrix")

  expect_silent(bb <- bma(list("a" = f1a, "b" = f1b, "d" = r1b),
    rel.tol = .01
  ))
  mean_avg <- sum(bb$posterior_models * bb$estimates[-1, "mean"])
  expect_equal(mean_avg, bb$estimates["averaged", "mean"], tolerance = .01)

  expect_is(bb$logml, "numeric")
  expect_is(bb$BF, "matrix")
  expect_is(bb$estimates, "matrix")


  skip_on_cran()
  suppressWarnings(r1a <- meta_random(yyy, SE, study,
    data = dat,
    summarize = "int",
    rel.tol = .01, iter = 100
  ))
  expect_silent(bb <- bma(list("a" = r1a, "b" = r1b), rel.tol = .Machine$double.eps^.1))
  postprob <- unname(bb$posterior_models[c(2, 4)])
  expect_equal(postprob / sum(postprob), c(.5, .5), tolerance = .01)
  expect_equal(r1a$estimates[, 1:7], r1b$estimates[, 1:7], tolerance = .05)
})


test_that("meta_bma gives identical results for stan/integrate", {
  skip_on_cran()
  set.seed(12352)
  mf_stan <- meta_bma(yyy, SE, study, dat,
    summarize = "stan", logml = "stan",
    logml_iter = 5000, iter = 10000
  )
  suppressWarnings({
    mf_int <- meta_bma(yyy, SE, study, dat,
      summarize = "int",
      logml = "int", iter = 100,
      rel.tol = .05
    )
  })
  expect_equal(mf_stan$estimates[, 1:7], mf_int$estimates[, 1:7], tolerance = .03)
  expect_equal(mf_stan$BF, mf_int$BF, tolerance = .01)
  expect_equal(mf_stan$inclusion, mf_int$inclusion, tolerance = .01)

  expect_silent(plot_forest(mf_stan))
  expect_silent(plot_forest(mf_int))
  expect_silent(plot_posterior(mf_int, from = 0, to = 1))
  expect_silent(plot_posterior(mf_stan))
})


test_that("inclusion() works correctly", {
  expect_silent(f1a <- meta_fixed(yyy, SE, study, data = dat, chains = 1, rel.tol = .01))
  expect_silent(f1b <- meta_fixed(yyy ~ 1, SE, study,
    data = dat, iter = 1750,
    logml = "stan", summarize = "stan", rel.tol = .01
  ))

  expect_silent(bb0 <- bma(list("a" = f1a, "b" = f1b), rel.tol = .01))
  expect_silent(bb1 <- inclusion(list("a" = f1a, "b" = f1b), include = c(2, 4)))
  expect_silent(bb2 <- inclusion(list("a" = f1a, "b" = f1b), include = "H1"))

  expect_is(bb1$prior, "numeric")
  expect_is(bb1$posterior, "numeric")
  expect_is(bb1$incl.prior, "numeric")
  expect_is(bb1$incl.posterior, "numeric")
  expect_is(bb1$incl.BF, "numeric")
  expect_is(bb1$include, "numeric")

  expect_equal(bb0$posterior_models, bb1$posterior)
  expect_equal(bb0$posterior_models, bb2$posterior)
  expect_equal(bb0$BF["a.fixed_H1", "a.fixed_H0"], bb2$incl.BF, tolerance = .001)
  expect_equal(bb1$incl.BF, bb2$incl.BF)
})
