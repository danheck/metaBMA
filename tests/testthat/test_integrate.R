library(metaBMA)
library(testthat)
library(rstan)

data(towels)

test_that("check old prior labels with logml='stan'/'integrate'", {
  set.seed(12345)

  # check different ways of defining priors
  m1 <- meta_fixed(logOR, SE, study, towels,
    d = prior("t", c(0, .1, 1), lower = 0), rel.tol = .01
  )
  suppressWarnings(
    m2 <- meta_fixed(logOR, SE, study, towels,
      d = prior("halfcauchy", .1), iter = 300, logml = "stan"
    )
  )
  expect_equal(m1$estimates[, 1:7, drop = FALSE], m2$estimates[, 1:7, drop = FALSE], tolerance = .005)
  expect_equal(m1$logml, m2$logml, tolerance = .01)

  skip_on_cran()

  suppressWarnings({
    mr1 <- meta_random(logOR, SE, study, towels,
      summarize = "int", rel.tol = .01, logml_iter = 100, iter = 1000,
      d = prior("t", c(0, .1, 5), lower = 0),
      tau = prior("norm", c(0, .3), lower = 0)
    )
    mr2 <- meta_random(logOR, SE, study, towels,
      summarize = "stan", iter = 1000, logml_iter = 1000,
      d = prior("scaledt", c(0, .1, 5), lower = 0),
      tau = prior("halfnorm", .3), logml = "stan"
    )
  })
  expect_equal(mr1$estimates[, 1:7], mr2$estimates[, 1:7], tolerance = .03)
  expect_equal(mr1$logml, mr2$logml, tolerance = .03)
})



test_that("extreme priors/misspecified models still provide correct results", {

  skip_on_cran()

  set.seed(12345)
  mf <- meta_fixed(logOR, SE, study, towels,
    rel.tol = .01,
    d = prior("norm", c(mean = 0.2, sd = .01))
  )
  expect_equal(mf$estimates[1:2], c(.2, .01), tolerance = .005)

  mr <- meta_random(logOR, SE, study, towels,
    rel.tol = .1, iter = 1000, logml_iter = 10,
    d = prior("norm", c(mean = 0.2, sd = .01))
  )
  expect_equal(mr$estimates[1], .2, tolerance = .005)

  suppressWarnings({
    mr <- meta_random(logOR, SE, study, towels,
      rel.tol = .005, iter = 1000,
      d = prior("norm", c(mean = 0.2, sd = .01)),
      tau = prior("t", c(.5, .01, 2), lower = .2)
    )
  })
  expect_equal(mr$estimates[1], .2, tolerance = .005)
  expect_silent(plot_posterior(mr, "d", from = .1, to = .3))
  expect_silent(plot_posterior(mr, "tau", from = .4, to = .6))
})
