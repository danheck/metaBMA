library(rstan)
library(testthat)
library(metaBMA)

set.seed(123452)
SE <- runif(20, .3,1.3)
d <- data.frame(yyy = rnorm(20, 0, .5), SE = SE, study = 1:20)
d$xx <- rnorm(20, 0, 1)
d$cat <- rep(c("a", "b"), 10)

test_that("meta_fixed: logml for H0 is correct", {

  f1 <- meta_fixed(yyy, SE, study, data = d, logml = "int")
  f1a <- meta_fixed(yyy ~ 1, SE, study, data = d, logml = "int")
  expect_silent(bb <- bma(list(a = f1, b = f1a)))
  expect_equal(bb$posterior_models, c(a = .5, b = .5))

  # logml with Stan
  f2 <- meta_fixed(yyy, SE, study, data = d, logml = "stan", iter = 6000, warmup = 1000)
  expect_equal(f1$logml, f2$logml, tolerance = .01)

  # different loglik for JZS
  expect_warning(f3 <- meta_fixed(yyy ~ xx, SE, study, data = d,   # warning: JZS
                                  logml = "stan", iter = 6000, warmup = 1000))
  expect_equal(f1$logml, f2$logml, tolerance = .01)
  expect_true(f1$logml > f3$logml + 1)
})


test_that("meta_random: logml for H0 is correct", {

  f1 <- meta_random(yyy, SE, study, data = d, logml = "int")
  f2 <- meta_random(yyy, SE, study, data = d, logml = "stan", summ = "s",
                    iter = 10000, logml_iter = 20000)
  expect_equal(f1$logml, f2$logml, tolerance = .02)
  expect_equal(f1$BF, f2$BF, tolerance = .01)

  # check: logml for "random_H0" with bridgesampling
  dl <- f1$data
  dl$model <- "random_H0"
  tmp <- capture_output(stanfit <- metaBMA:::meta_stan(dl, iter = 30000))
  bs <- bridgesampling:::bridge_sampler(stanfit, silent = TRUE, use_neff = FALSE)
  # expect_equal(bs$logml, f1$logml - log(f1$BF[["d_10"]]), tolerance = .001)

  # TODO: check random_H0 logml  / BF

  # f1$posterior_tau(0) / f1$prior_tau(0)
  # hist(extract(stanfit)$tau, 100, freq=F)
  # curve(f2$prior_tau(x), add=T, lwd=2)
})
