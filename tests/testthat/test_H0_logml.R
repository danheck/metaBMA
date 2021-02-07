library(rstan)
library(testthat)
library(metaBMA)

set.seed(12352)
SE <- runif(20, .3, 1.3)
dat <- data.frame(yyy = rnorm(20, 0, .5), SE = SE, study = 1:20)
dat$xx <- rnorm(20, 0, 1)
dat$cat <- rep(c("a", "b"), 10)

test_that("meta_fixed: logml for H0 is correct", {
  f1 <- meta_fixed(yyy, SE, study, data = dat, logml = "int", rel.tol = .1)
  f1a <- meta_fixed(yyy ~ 1, SE, study, data = dat, logml = "int", rel.tol = .1)
  expect_silent(bb <- bma(list(a = f1, b = f1a), rel.tol = .1))
  postprob <- unname(bb$posterior_models[c(2, 4)])
  expect_equal(postprob / sum(postprob), c(.5, .5))

  # logml with Stan
  suppressWarnings(
    f2 <- meta_fixed(yyy, SE, study,
      data = dat, logml = "stan",
      iter = 1450, warmup = 200, rel.tol = .1
    )
  )
  expect_equal(f1$logml, f2$logml, tolerance = .01)
  expect_equal(f1$BF, f1a$BF, tolerance = .1)
  expect_equal(metaBMA:::loglik_fixed_H0(f1$data),
    unname(f2$logml[1]),
    tolerance = .01
  )

  # different loglik for JZS
  expect_warning(f3 <- meta_fixed(yyy ~ xx, SE, study,
    data = dat, # warning: JZS
    logml = "stan", iter = 1450, warmup = 200, rel.tol = .1
  ))
  expect_true(f1$logml["fixed_H1"] > f3$logml["fixed_H1"] + 1)
})

test_that("meta_random: logml & estimates correct", {
  skip_on_cran()

  suppressWarnings(
    f1 <- meta_random(yyy, SE, study,
      data = dat,
      iter = 100, logml_iter = 100, logml = "int", summ = "int", rel.tol = .1
    )
  )
  # suppressWarnings( # few samples for bridgesampling
  f2 <- meta_random(yyy, SE, study,
    data = dat, logml = "stan", summ = "stan",
    iter = 500, logml_iter = 1450, warmup = 200, rel.tol = 1
  )
  # )
  expect_equal(f1$logml, f2$logml, tolerance = .001)
  expect_equal(f1$BF, f2$BF, tolerance = .1)
  expect_equal(f1$estimates[, 1:7], f2$estimates[, 1:7], tolerance = .03)
  expect_equal(f1$posterior_d(0) / f1$prior_d(0),
    f2$BF["random_H0", "random_H1"],
    tolerance = .1
  )

  # check: logml for "random_H0" with bridgesampling
  stanfit <- metaBMA:::meta_stan(f1$data, d = prior("0"), iter = 1000)
  bs <- bridgesampling:::bridge_sampler(stanfit, silent = TRUE, use_neff = FALSE)
  expect_equal(bs$logml, unname(f1$logml["random_H0"]), tolerance = .003)
})

test_that("Stan models 'random_dstudy' and 'random' are equivalent", {
  dl <- list(y = dat$yyy, SE = dat$SE, N = length(dat$SE))

  dl$model <- "random"
  suppressWarnings(stanfit1 <- metaBMA:::meta_stan(dl, iter = 500))
  bs1 <- bridgesampling:::bridge_sampler(stanfit1, silent = TRUE, use_neff = FALSE)

  dl$model <- "random_dstudy"
  suppressWarnings(stanfit2 <- metaBMA:::meta_stan(dl, iter = 500))
  bs2 <- bridgesampling:::bridge_sampler(stanfit2, silent = TRUE, use_neff = FALSE)

  expect_equal(bs1$logml, bs2$logml, tolerance = .1)
  sel <- c("mean", "sd", "25%", "50%", "75%")
  expect_equal(summary(stanfit1, c("d", "tau"))$summary[, sel],
    summary(stanfit2, c("d", "tau"))$summary[, sel],
    tolerance = .03
  )
})
