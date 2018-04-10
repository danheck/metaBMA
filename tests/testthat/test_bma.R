

test_that("that averaging works", {
  data(towels)
  set.seed(12345)
  eval(data(towels))

  expect_silent(mf_jags <- meta_bma(logOR, SE, study, towels, sample = 5000,
                                    summarize = "jags"))
  expect_silent(mf_int <- meta_bma(logOR, SE, study, towels, sample = 0,
                                   summarize = "int"))
  expect_equal(mf_jags$estimates, mf_int$estimates, tolerance = .03)

  expect_silent(plot_forest(mf_jags))
  expect_silent(plot_forest(mf_int))
  expect_silent(plot_posterior(mf_int))
  expect_silent(plot_posterior(mf_jags))

  expect_silent(mf_jags <- meta_default(logOR, SE, study, towels, sample = 5000,
                                        summarize = "jags"))
  expect_silent(mf_int <- meta_default(logOR, SE, study, towels, sample = 0,
                                       summarize = "int"))

  expect_equal(mf_jags$estimates, mf_int$estimates, tolerance = .03)
})

