library("metaBMA")
library("rstan")
library("testthat")

set.seed(123452)
d <- data.frame(yyy = rnorm(10), se = runif(10), xx = rnorm(10), study = 1:10)

test_that("data objects are correctly evaluated", {

  expect_silent(fit <- meta_fixed(d$yyy, "se", "study", d, sample = 0))
  expect_silent(fit <- meta_fixed(d$yyy, d$se, d$study, sample = 0))
  expect_silent(fit <- meta_fixed(d$yyy, d$se, data = d, sample = 0))
  expect_silent(fit <- meta_fixed(yyy, se, study, d, sample = 0))

  expect_silent(fit <- meta_fixed("yyy", "se", "study", d, sample = 0))
  expect_silent(fit <- meta_fixed("yyy", se, study, d, sample = 0))
  expect_silent(fit <- meta_fixed(yyy, se, study, d, sample = 0))


  expect_silent(fit <- meta_stan("yyy", "se", "study", d, iter = 50))
  expect_silent(fit <- meta_stan("yyy", se, study, d, iter = 50))
  expect_silent(fit <- meta_stan(yyy, se, study, d, iter = 50))

  # formula interface
  expect_silent(fit <- meta_stan(d$yyy, d$se, d$study, iter = 50))
  expect_silent(fit <- meta_stan(yyy ~ 1, se, study, d, iter = 50))
  expect_silent(fit <- meta_stan(yyy ~ 1, se, study, d, iter = 50, model = "f"))
  expect_silent(fit <- meta_stan(yyy ~ xx, se, study, d, iter = 50, model = "f"))
  expect_silent(fit <- meta_stan(yyy ~ xx, se, study, d, iter = 50, model = "r"))

})


