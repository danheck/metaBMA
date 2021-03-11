library(metaBMA)
library(testthat)

set.seed(123)
SE <- runif(10, .3, 1.3)
dat <- data.frame(yyy = rnorm(10), SE = SE, xx = rnorm(10), study = 1:10)
data_list <- list(
  model = "random", N = nrow(dat),
  y = dat$yyy, SE = dat$SE, labels = dat$study,
  data = NULL, model.frame = NULL
)


test_that("priors for stan models are properly checked", {
  expect_output(suppressWarnings(
    ms <- metaBMA:::meta_stan(data_list, iter = 50, cores = 1, silent_stan = FALSE)
  ))
  expect_warning(ms <- metaBMA:::meta_stan(data_list, iter = 50, cores = 1))

  expect_true(class(ms) == "stanfit")
  suppressWarnings(ms <- meta_random(yyy, SE, data = dat, iter = 50, logml_iter = 50))
  expect_true(class(ms) == "meta_random")
  expect_true(class(ms$stanfit) == "stanfit")
  expect_true(class(ms$stanfit_dstudy) == "stanfit")
  expect_error(metaBMA:::meta_stan(data_list, d = prior("dsad"), iter = 50))
  expect_error(metaBMA:::meta_stan(data_list, prior("t", c(1, 0, 0)), iter = 50))
  expect_error(metaBMA:::meta_stan(data_list, prior("t", c(1, 3, -10)), iter = 50))
  expect_error(metaBMA:::meta_stan(data_list, prior("t", c(1, 3, 1.3)), iter = 50))
  expect_error(metaBMA:::meta_stan(data_list, prior("t", c(1, 3)), iter = 50))
  expect_error(metaBMA:::meta_stan(data_list, prior("norm", c(1, -1)), iter = 50))
})
