library(testthat)
library(metaBMA)


test_that("Scheibehenne (2017): model averaging for towels dataset works correctly", {
  data(towels, package = "metaBMA")
  tmp <- capture_output(eval(towels))

  ############ TESTING: scale = 1/4

  # d.rand ~ dnorm(0, 1/0.3^2)T(0,)
  # prec <- 1/(tau*tau)  # btw-study precision
  # tau ~  dt(0, prior.scaleTau, 1) T(0,)
  # sdVec <- c(1/64, 1/32, 1/16, 1/8, 1/4, 1/2, 1, 10, 100)   # TEST
  # scaleTauVec <- 1/sdVec^2
  expect_silent(prior_d <- prior(
    family = "norm", param = c(mean = 0, sd = 0.3),
    lower = 0, label = "d"
  ))
  expect_silent(prior_d_unrestricted <- prior(family = "norm", param = c(mean = 0, sd = 0.3)))
  expect_silent(prior_tau <- prior(
    family = "t", param = c(location = 0, scale = 1 / 4, nu = 1),
    lower = 0, label = "tau"
  ))

  set.seed(123)
  suppressWarnings( # divergent transitions
    m_testing <- meta_bma(
      y = logOR, SE = SE, data = towels,
      d = prior_d, tau = prior_tau,
      iter = 1500, logml_iter = 2000, rel.tol = .1, summ = "stan"
    )
  )

  BF_fixed_random <- 2.450923
  BFplus0_random <- 4.055704
  BFplus0_fixed <- 23.88449
  BFincl <- 9.882879

  expect_equal(m_testing$BF["fixed_H1", "random_H1"], BF_fixed_random, tolerance = .03)
  expect_equal(m_testing$BF["random_H1", "random_H0"], BFplus0_random, tolerance = .01)
  expect_equal(m_testing$BF["fixed_H1", "fixed_H0"], BFplus0_fixed, tolerance = .01)
  expect_equal(m_testing$inclusion$incl.BF, BFincl, tolerance = .01)


  skip_on_cran()

  ############ ESTIMATION

  m.fixed <- 0.2116763
  m.rand <- 0.1848269
  m.mixed <- 0.2033756
  hpd95.fixed <- c(0.063656, 0.3623073)
  hpd95.rand <- c(-0.02162486, 0.38411546)
  hpd95.mixed <- c(0.03867814, 0.37240522)

  suppressWarnings(
    meta_est <- meta_bma(
      y = logOR, SE = SE, data = towels,
      d = prior_d_unrestricted, tau = prior_tau,
      iter = 1500, logml_iter = 1000, rel.tol = .1, summ = "stan"
    )
  )

  expect_equal(meta_est$estimates["fixed", "mean"], m.fixed, tolerance = .01)
  expect_equal(unname(meta_est$estimates["fixed", c("hpd95_lower", "hpd95_upper")]),
    hpd95.fixed,
    tolerance = .03
  )
  expect_equal(meta_est$estimates["random", "mean"], m.rand, tolerance = .01)
  expect_equal(unname(meta_est$estimates["random", c("hpd95_lower", "hpd95_upper")]),
    hpd95.rand,
    tolerance = .03
  )
  expect_equal(meta_est$estimates["averaged", "mean"], m.mixed, tolerance = .01)
  expect_equal(unname(meta_est$estimates["averaged", c("hpd95_lower", "hpd95_upper")]),
    hpd95.mixed,
    tolerance = .03
  )
})
