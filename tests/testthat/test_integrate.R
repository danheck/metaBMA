
test_that("extreme priors/misspecified models provide correct results with integrate/JAGS/stan", {
  data(towels)
  set.seed(12345)
  library("rstan")

  mf_jags <- meta_fixed(logOR, SE, study, towels, sample = 10000,
                        d.par = c(mean = 0.2, sd = .01), summarize = "jags")
  expect_silent(mf_int <- meta_fixed(logOR, SE, study, towels,
                                     d.par = c(mean = 0.2, sd = .01)))
  expect_silent(mf_stan <- meta_stan(logOR, SE, study, towels,  model = "fixed",
                                     d = c(1e5, .2, .01, -Inf, Inf), iter = 10000))
  expect_equal(mf_jags$estimates, mf_int$estimates, tolerance = .01)
  expect_equal(unname(mf_int$estimates[,1:5]),
               unname(summary(mf_stan)$summary["d",c("mean", "50%", "sd", "2.5%", "97.5%")]),
               tolerance = .01)

  expect_silent(mf_int <- meta_random(logOR, SE, study, towels,
                                      d.par = c(mean = 0.2, sd = .01)))
  mf_jags <- meta_random(logOR, SE, study, towels,
                         d.par = c(mean = 0.2, sd = .01), summ = "j", sample = 10000)
  expect_silent(mf_stan <- meta_stan(logOR, SE, study, towels,  model = "random",
                                     d = c(1e5, .2, .01, -Inf, Inf),
                                     tau = c(1, 0, .5, 0, Inf), iter = 10000))
  expect_equal(mf_jags$estimates, mf_int$estimates, tolerance = .05)
  expect_equal(unname(mf_int$estimates[,1:5]),
               unname(summary(mf_stan)$summary[c("d", "tau"),
                                               c("mean", "50%", "sd", "2.5%", "97.5%")]),
               tolerance = .05)
})



test_that("extreme priors/misspecified models still provide correct results", {
  expect_silent(meta_fixed(logOR, SE, study, towels,  d.par = c(mean = 0.2, sd = .01)))
  expect_silent(meta_random(logOR, SE, study, towels, d.par = c(mean = 0.2, sd = .05)))
})
