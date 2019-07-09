library(testthat)
library(metaBMA)

set.seed(123)

test_that("Gronau (2017): power pose analysis (reported) are correct ", {
  data(power_pose, package = "metaBMA")
  tmp <- capture_output(eval(power_pose))

  expect_silent(
    priorEStesting <- prior(family = "t",
                            param = c(location = 0, scale = 1/sqrt(2), nu = 1),
                            lower = 0, label = "d"))
  expect_silent(
    priorESestimation <- prior(family = "t",
                               param = c(location=0, scale=1/sqrt(2), nu=1),
                               label = "d"))
  expect_silent(
    priorTau <- prior("invgamma", c(1, .15), label = "tau"))

  suppressWarnings(  #divergent transitions
    m_testing <- meta_bma(y = effectSize, SE = SE, d = priorEStesting, data = power_pose,
                          tau = priorTau, iter = 100, logml_iter = 2000, rel.tol = .1, summ = "stan"))

  bf_reported <- c("d_10_fixed" = 89.570, "d_10_random" = 9.374)
  expect_equal(unname(unlist(m_testing$BF)[c(2,12)]),
               unname(bf_reported), tolerance = .0001)
  expect_equal(m_testing$inclusion$incl.BF, 33.136, tolerance = .001)

  suppressWarnings(  #divergent transitions
    m_estimation <- meta_bma(y = effectSize, SE = SE, data =power_pose, d = priorESestimation,
                             tau = priorTau, iter = 2000, rel.tol = .1,
                             logml_iter = 100, summarize = "stan"))
  expect_equal(m_estimation$estimates["averaged",c(4, 6:7)],
               # c("50%" = 0.22, "hpd95_lower" = 0.09, "hpd95_upper" = 0.34), # summ="int"
               c("50%" = 0.22, "hpd95_lower" = 0.06, "hpd95_upper" = 0.36), # summ="stan"
               tolerance = .03)
})


test_that("Gronau (2017): power pose analysis with informed t prior ", {
  skip_on_cran()
  data(power_pose, package = "metaBMA")
  tmp <- capture_output(eval(power_pose))

  priorEStesting <- prior(family = "t",
                          param = c(location = 0.34999, scale = 0.1021, nu = 3),
                          lower = 0, label = "d")
  priorESestimation <- prior(family = "t",
                             param = c(location = 0.34999, scale = 0.1021, nu = 3),
                             label = "d")
  priorTau <- prior("invgamma", c(1, .15), label = "tau")

  # conduct analyses
  m_testing <- meta_bma(y = effectSize, SE = SE, d = priorEStesting, data = power_pose,
                        tau = priorTau, iter = 100)
  bf_reported <- c("d_10_fixed" = 191.751, "d_10_random" = 20.689)
  expect_equal(unname(unlist(m_testing$BF)[c(2,12)]), unname(bf_reported), tolerance = .00001)
  expect_equal(m_testing$inclusion$incl.BF, 71.373, tolerance = .001)

  m_estimation <- meta_bma(y = effectSize, SE = SE, data =power_pose, d = priorESestimation,
                           tau = priorTau, iter = 1000, summ = "int")
  expect_equal(m_estimation$estimates["averaged",c(4, 6:7)],
               c("50%" = 0.26, "hpd95_lower" = 0.14, "hpd95_upper" = 0.37), tolerance = .01)
})



test_that("Gronau (2017): power pose analysis (only unfamiliar with default prior)", {
  skip_on_cran()
  # data(power_pose_unfamiliar, package = "metaBMA")
  tmp <- capture_output(eval(power_pose_unfamiliar))

  priorEStesting <- prior(family = "t", param = c(location = 0, scale = 1/sqrt(2), nu = 1),
                          lower = 0, label = "d")
  priorESestimation <- prior(family = "t", param = c(location=0, scale=1/sqrt(2), nu=1), label = "d")
  priorTau <- prior("invgamma", c(1, .15), label = "tau")

  # conduct analyses
  m_testing <- meta_bma(y = effectSize, SE = SE, d = priorEStesting, data = power_pose_unfamiliar,
                        tau = priorTau, iter = 100)
  bf_reported <- c("d_10_fixed" = 4.449, "d_10_random" = 1.640)
  expect_equal(unname(unlist(m_testing$BF)[c(2,12)]), unname(bf_reported), tolerance = .001)
  expect_equal(m_testing$inclusion$incl.BF, 3.139, tolerance = .001)

  m_estimation <- meta_bma(y = effectSize, SE = SE, data = power_pose_unfamiliar,
                           d = priorESestimation,
                           tau = priorTau, iter = 1000, summarize = "int")
  expect_equal(m_estimation$estimates["averaged",c(4, 6:7)],
               c("50%" = .18, "hpd95_lower" = .03, "hpd95_upper" = .33), tolerance = .01)
})


test_that("Gronau (2017): power pose analysis (only unfamiliar and informed t prior) ", {
  skip_on_cran()
  # data(power_pose_unfamiliar, package = "metaBMA")
  tmp <- capture_output(eval(power_pose_unfamiliar))

  priorEStesting <- prior(family = "t",
                          param = c(location = 0.34999, scale = 0.1021, nu = 3),
                          lower = 0, label = "d")
  priorESestimation <- prior(family = "t",
                             param = c(location = 0.34999, scale = 0.1021, nu = 3),
                             label = "d")
  priorTau <- prior("invgamma", c(1, .15), label = "tau")

  # conduct analyses
  m_testing <- meta_bma(y = effectSize, SE = SE, d = priorEStesting,
                        data = power_pose_unfamiliar,
                        tau = priorTau, iter = 100)
  bf_reported <- c("d_10_fixed" = 6.846, "d_10_random" = 2.603)
  expect_equal(unname(unlist(m_testing$BF)[c(2,12)]), unname(bf_reported), tolerance = .0001)
  expect_equal(m_testing$inclusion$incl.BF, 4.868, tolerance = .001)

  m_estimation <- meta_bma(y = effectSize, SE = SE, data =power_pose_unfamiliar,
                           d = priorESestimation,
                           tau = priorTau, iter = 1000, summ = "int")
  expect_equal(m_estimation$estimates["averaged",c(4, 6:7)],
               c("50%" = .23, "hpd95_lower" = .10, "hpd95_upper" = .36), tolerance = .02)
})

