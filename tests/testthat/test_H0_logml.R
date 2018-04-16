library("rstan")

set.seed(123452)
se <- runif(20, .3,1.3)
d <- data.frame(yyy = rnorm(20, 0, .5), se = se, study = 1:20)
d$xx <- rnorm(20, 0, 1)
d$cat <- rep(c("a", "b"), 10)


test_that("meta_fixed: logml for H0 is correct", {

  f1 <- meta_fixed(yyy, se, study, data = d, logml = "int")
  f1a <- meta_fixed(yyy ~ 1, se, study, data = d, logml = "int")
  expect_silent(bb <- bma(list(a = f1, b = f1a)))
  expect_equal(bb$posterior_models, c(a = .5, b = .5))

  # logml with Stan
  f2 <- meta_fixed(yyy, se, study, data = d, logml = "stan", iter = 6000, warmup = 1000)
  expect_equal(f1$logml, f2$logml, tolerance = .01)

  # different loglik for JZS
  expect_warning(f3 <- meta_fixed(yyy ~ xx, se, study, data = d,   # warning: JZS
                                  logml = "stan", iter = 6000, warmup = 1000))
  expect_equal(f1$logml, f2$logml, tolerance = .01)
  expect_true(f1$logml > f3$logml + 1)
})


test_that("meta_random: logml for H0 is correct", {

  f1 <- meta_random(yyy, se, study, data = d, logml = "int")
  f2 <- meta_random(yyy, se, study, data = d, logml = "stan", summ = "s", iter = 6000, warmup = 1000)
  expect_equal(f1$logml, f2$logml, tolerance = .03)
  expect_equal(f1$BF, f2$BF, tolerance = .05)

  # logml H0 with bridgesampling
  dl <- f1$data
  dl$model <- "random_H0"
  tmp <- capture_output(stanfit <- metaBMA:::meta_stan(dl))
  bs <- bridgesampling:::bridge_sampler(stanfit, silent = TRUE, use_neff = FALSE)
  expect_equal(bs$logml, f1$logml - log(f1$BF[[1]]), tolerance = .001)
})
