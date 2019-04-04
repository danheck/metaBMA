library(metaBMA)
library(testthat)
library(rstan)

data(towels)
set.seed(12345)

test_that("extreme priors/misspecified models provide correct results with integrate/JAGS/stan", {

  # check different ways of defining priors
  expect_silent(meta_fixed(logOR, SE, study, towels,
                           d = prior("t", c(0, .1, 1))))
  expect_silent(meta_random(logOR, SE, study, towels, summarize = "int",
                            d = prior("t", c(0, .1, 1)),
                            tau = prior("norm", c(0,.3), 0)))
  expect_silent(meta_fixed(logOR, SE, study, towels, d = prior("beta", c(1,1)), iter = 5000))

  d <- prior("norm", c(mean = 0.2, sd = .01))
  mf_stan <- meta_fixed(logOR, SE, study, towels, d= d, summarize = "stan", iter = 1e5)
  expect_silent(mf_int <- meta_fixed(logOR, SE, study, towels, d = d))
  expect_equal(mf_stan$estimates, mf_int$estimates, tolerance = .01)

  mr_stan <- meta_random(logOR, SE, study, towels, iter = 20000, d = d)
  expect_silent(mr_int <- meta_random(logOR, SE, study, towels, d = d, summ = "int"))
  expect_equal(mr_stan$estimates, mr_int$estimates, tolerance = .025)
})


test_that("extreme priors/misspecified models still provide correct results", {
  expect_silent(meta_fixed(logOR, SE, study, towels,
                           d = prior("norm", c(mean = 0.2, sd = .01))))
  expect_silent(meta_random(logOR, SE, study, towels,
                            d = prior("norm", c(mean = 0.2, sd = .05))))
})
