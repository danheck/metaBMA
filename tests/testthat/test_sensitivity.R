library(metaBMA)
library(testthat)

set.seed(123)

test_that("meta_sensitivity() expected results", {

  expect_silent(sens <- meta_sensitivity(
    logOR, SE, study, towels,
    d_list = list(prior("cauchy", c(0, .707)),
                  prior("norm", c(.5, .3))),
    tau_list = list(prior("invgamma", c(1, 0.15), label = "tau"),
                    prior("gamma", c(1.5, 3), label = "tau")),
    analysis = "fixed", combine_priors = "matched"))
  expect_length(sens, 2)
  expect_type(sens, "list")
  expect_output(print(sens))
  expect_output(plot(sens))
  expect_output(plot(sens, distribution = "prior", from = -2, to = 2, n = 31))


  skip_on_cran()
  sens <- meta_sensitivity(
    logOR, SE, study, towels,
    d_list = list(prior("cauchy", c(0, .707)),
                  prior("norm", c(.5, .3))),
    tau_list = list(prior("cauchy", c(0,.5), lower = 0, label = "tau"),
                    prior("gamma", c(1.5, 3), label = "tau")),
    analysis = "random", combine_priors = "matched",
    control = list(adapt_delta = .99))
  expect_length(sens, 2)
  expect_type(sens, "list")
  expect_output(print(sens))


  skip_on_cran()
  sens <- meta_sensitivity(
    logOR, SE, study, towels,
    d_list = list(prior("cauchy", c(0, .707)),
                  prior("norm", c(.5, .3))),
    tau_list = list(prior("cauchy", c(0,.5), lower = 0, label = "tau"),
                    prior("gamma", c(1.1, 3), label = "tau")),
    analysis = "bma", combine_priors = "crossed",
    control = list(adapt_delta = .98))
  expect_length(sens, 2*2)
  expect_type(sens, "list")
  expect_output(print(sens))
  expect_output(plot(sens, parameter = "d"))
  expect_output(plot(sens, distr = "prior", parameter = "d"))
  expect_warning(plot(sens, parameter = "tau"))
  expect_warning(plot(sens, distr = "prior", parameter = "tau"))


})



