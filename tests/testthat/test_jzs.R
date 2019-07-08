library(metaBMA)
library(testthat)

set.seed(123452)
SE <- runif(20, .3,1.3)
d <- data.frame(yyy = rnorm(20), SE = SE, study = 1:20)
d$xx <- rnorm(20, d$yyy, 1)
d$cat <- rep(c("a", "b"), 10)


test_that("JZS works for continuous predictors", {

  fe_xx <- meta_fixed(yyy ~ xx, SE, study, data = d, logml= "s", summ = "s", iter = 10000)
  re_xx <- meta_random(yyy ~ xx, SE, study, data = d, logml= "s", summ = "s", iter = 10000)

  # default: centering
  expect_true(fe_xx$jzs$centering)
  expect_true(re_xx$jzs$centering)

  # posterior summary statistics for slope parameters
  expect_equal(rownames(fe_xx$estimates), c("d", "alpha_xx"))
  expect_equal(rownames(re_xx$estimates), c("d", "tau", "alpha_xx"))

  plot_posterior(fe_xx)
  plot_posterior(re_xx)

  expect_silent(bm <- bma(list("fixed" = fe_xx, "random" = re_xx), summ="integrate"))
  # expect_silent(bm <- bma(list("fixed" = fe_xx, "random" = re_xx), parameter = "alpha_xx", summ="stan"))
  expect_silent(plot_posterior(bm))

  expect_silent(bm <- inclusion(list("fixed" = fe_xx, "random" = re_xx), include = "H1"))

  s1 <- summary(re_xx$stanfit)$summary[c("d", "tau", "alpha[1]"), c(1,3:8)]
  s2 <- summary(re_xx$stanfit_dstudy)$summary[c("d", "tau", "alpha[1]"), c(1,3:8)]
  expect_equal(s1, s2, tolerance = .04)

  # compare meta_random() to metafor::rma()
  # d$xx.c <- scale(d$xx, scale = FALSE)
  # metafor::rma(yyy, sei = SE, mod = ~ xx, data = d)
})


test_that("JZS works for discrete predictors", {

  fe_xx <- meta_fixed(yyy ~ cat, SE, study, data = d, logml= "s", summ = "s", iter = 6000)
  re_xx <- meta_random(yyy ~ cat, SE, study, data = d, logml= "s", summ = "s", iter = 6000)

  expect_equal(rownames(fe_xx$estimates), c("d", "alpha_cat1"))
  expect_equal(rownames(re_xx$estimates), c("d", "tau", "alpha_cat1"))

  expect_silent(bm <- bma(list(fe_xx, re_xx)))
  s1 <- summary(re_xx$stanfit)$summary[c("d", "tau", "alpha[1]"), c(1,3:8)]
  s2 <- summary(re_xx$stanfit_dstudy)$summary[c("d", "tau", "alpha[1]"), c(1,3:8)]
  expect_equal(s1, s2, tolerance = .02)
})


test_that("JZS works for continuous + discrete predictors", {

  fe_both <- meta_fixed(yyy ~ cat + xx, SE, study, data = d, logml= "s",
                        summ = "s", iter = 6000)
  expect_equal(rownames(fe_both$estimates), c("d",  "alpha_xx", "alpha_cat1"))

})
