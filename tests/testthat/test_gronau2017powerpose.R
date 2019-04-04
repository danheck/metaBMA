library(testthat)
library(metaBMA)

test_that("Gronau (2017): power pose analysis (reported) are correct ", {
  data(power_pose)
  tmp <- capture_output(eval(power_pose))

  priorEStesting <- prior(family = "t", param = c(location = 0, scale = 1/sqrt(2), nu = 1),
                          lower = 0, label = "d")
  priorESestimation <- prior(family = "t", param = c(location=0, scale=1/sqrt(2), nu=1), label = "d")
  priorTau <- prior("invgamma", c(1, .15), label = "tau")

  m_testing <- meta_bma(y = effectSize, SE = SE, d = priorEStesting, data = power_pose,
                        tau = priorTau, iter = 1000, summ = "int")
  bf_reported <- c(d_10_fixed = 89.570, d_10_random = 9.374, d_10_averaged = 33.136)
  expect_equal(unname(unlist(m_testing$BF)), unname(bf_reported), tolerance = .0001)

  m_estimation <- meta_bma(y = effectSize, SE = SE, data =power_pose, d = priorESestimation,
                           tau = priorTau, iter = 1000, summ = "int")
  expect_equal(m_estimation$estimates["Averaged",c(4, 6:7)],
               c("50%" = 0.22, "hpd95_lower" = 0.09, "hpd95_upper" = 0.34), tolerance = .01)
})


test_that("Gronau (2017): power pose analysis with informed t prior ", {
  data("power_pose")
  tmp <- capture_output(eval(power_pose))

  priorEStesting <- prior(family = "t", param = c(location = 0.34999, scale = 0.1021, nu = 3),
                          lower = 0, label = "d")
  priorESestimation <- prior(family = "t", param = c(location = 0.34999, scale = 0.1021, nu = 3),
                             label = "d")
  priorTau <- prior("invgamma", c(1, .15), label = "tau")

  # conduct analyses
  m_testing <- meta_bma(y = effectSize, SE = SE, d = priorEStesting, data = power_pose,
                        tau = priorTau, iter = 1000, summ = "int")
  bf_reported <- c(d_10_fixed = 191.751, d_10_random = 20.689, d_10_averaged = 71.373)
  expect_equal(unname(unlist(m_testing$BF)), unname(bf_reported), tolerance = .00001)

  m_estimation <- meta_bma(y = effectSize, SE = SE, data =power_pose, d = priorESestimation,
                           tau = priorTau, iter = 1000, summ = "int")
  expect_equal(m_estimation$estimates["Averaged",c(4, 6:7)],
               c("50%" = 0.26, "hpd95_lower" = 0.14, "hpd95_upper" = 0.37), tolerance = .01)
})



test_that("Gronau (2017): power pose analysis (only unfamiliar with default prior)", {
  data("power_pose_unfamiliar", "metaBMA")
  tmp <- capture_output(eval(power_pose_unfamiliar))

  priorEStesting <- prior(family = "t", param = c(location = 0, scale = 1/sqrt(2), nu = 1),
                          lower = 0, label = "d")
  priorESestimation <- prior(family = "t", param = c(location=0, scale=1/sqrt(2), nu=1), label = "d")
  priorTau <- prior("invgamma", c(1, .15), label = "tau")

  # conduct analyses
  m_testing <- meta_bma(y = effectSize, SE = SE, d = priorEStesting, data = power_pose_unfamiliar,
                        tau = priorTau, iter = 1000, summ = "int")
  bf_reported <- c(d_10_fixed = 4.449, d_10_random = 1.640, d_10_averaged = 3.139)
  expect_equal(unname(unlist(m_testing$BF)), unname(bf_reported), tolerance = .001)

  m_estimation <- meta_bma(y = effectSize, SE = SE, data =power_pose_unfamiliar, d = priorESestimation,
                           tau = priorTau, iter = 1000, summ = "int")
  expect_equal(m_estimation$estimates["Averaged",c(4, 6:7)],
               c("50%" = .18, "hpd95_lower" = .03, "hpd95_upper" = .33), tolerance = .01)
})


test_that("Gronau (2017): power pose analysis (only unfamiliar and informed t prior) ", {
  data("power_pose_unfamiliar", "metaBMA")
  tmp <- capture_output(eval(power_pose_unfamiliar))

  priorEStesting <- prior(family = "t", param = c(location = 0.34999, scale = 0.1021, nu = 3),
                          lower = 0, label = "d")
  priorESestimation <- prior(family = "t", param = c(location = 0.34999, scale = 0.1021, nu = 3),
                             label = "d")
  priorTau <- prior("invgamma", c(1, .15), label = "tau")

  # conduct analyses
  m_testing <- meta_bma(y = effectSize, SE = SE, d = priorEStesting, data = power_pose_unfamiliar,
                        tau = priorTau, iter = 1000, summ = "int")
  bf_reported <- c(d_10_fixed = 6.846, d_10_random = 2.603, d_10_averaged = 4.868)
  expect_equal(unname(unlist(m_testing$BF)), unname(bf_reported), tolerance = .0001)

  m_estimation <- meta_bma(y = effectSize, SE = SE, data =power_pose_unfamiliar, d = priorESestimation,
                           tau = priorTau, iter = 1000, summ = "int")
  expect_equal(m_estimation$estimates["Averaged",c(4, 6:7)],
               c("50%" = .23, "hpd95_lower" = .10, "hpd95_upper" = .36), tolerance = .01)
})
