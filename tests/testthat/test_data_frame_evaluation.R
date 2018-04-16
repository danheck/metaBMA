library("testthat")
set.seed(1235)
se <- runif(10, .3,1.3)
d <- data.frame(yyy = rnorm(10), se = se, study = 1:10,
                xx = rnorm(10), cat = c("a", "b"))

test_that("meta_fixed supports data as vector/variable names", {

  # vectors:
  fit1 <- meta_fixed(d$yyy, d$se, d$study, control=list(adapt_delta=.9))
  fit2 <- meta_fixed(d[["yyy"]], d[["se"]], d[["study"]], control=list(adapt_delta=.9))
  fit3 <- meta_fixed(d$yyy, d$se, d$study, control=list(adapt_delta=.9))
  expect_identical(fit1$logml, fit2$logml)
  expect_identical(fit1$logml, fit3$logml)
  expect_identical(fit1$estimates, fit3$estimates)

  # (un)quoted variable names
  fit4 <- meta_fixed("yyy", "se", "study", d, control=list(adapt_delta=.9))
  fit5 <- meta_fixed("yyy", se, study, d, control=list(adapt_delta=.9))
  fit6 <- meta_fixed(yyy, se, study, d, control=list(adapt_delta=.9))
  expect_identical(fit1$logml, fit4$logml)
  expect_identical(fit1$logml, fit5$logml)
  expect_identical(fit1$logml, fit6$logml)
  expect_identical(fit1$estimates, fit4$estimates)
  expect_identical(fit1$estimates, fit5$estimates)
  expect_identical(fit1$estimates, fit6$estimates)
})


test_that("meta_bma supports data as vector/variable names", {

  fit7 <- meta_bma("yyy", "se", "study", d, control=list(adapt_delta=.9), summ="i")
  fit8 <- meta_bma(yyy, se, study, d, rel.tol = .1, iter = 6000,warmup=1000,
                   control=list(adapt_delta=.9), summ = "s")
  fit9 <- meta_bma("yyy", se, study, d, control=list(adapt_delta=.9), iter = 10000, warmup=1000, logml="s")
  fit10 <- meta_bma(yyy, se, study, d, control=list(adapt_delta=.9), iter = 6000, warmup=1000)

  expect_equal(fit7$logml, fit8$logml)
  expect_identical(fit7$logml, fit10$logml)
  expect_equal(fit7$logml, fit9$logml, tolerance = .001)

  expect_equal(fit7$estimates, fit8$estimates, tolerance = .03)
  expect_equal(fit7$estimates, fit9$estimates, tolerance = .01)
  expect_equal(fit7$estimates, fit10$estimates, tolerance = .01)
})


test_that("meta_fixed supports y as formula", {
  fit <- meta_fixed(yyy, se, data = d, study, control=list(adapt_delta=.9))
  fit2 <- meta_fixed(yyy ~ 1, se, data = d, study, control=list(adapt_delta=.9))
  expect_equal(fit$logml, fit2$logml)
})


test_that("JZS prior is correctly defined via formula", {
  fit1 <- meta_fixed(yyy ~ xx, se, study, d, iter = 6000, warmup = 1000,
                     logml="s", summ="s", control=list(adapt_delta=.9))
  expect_identical(rownames(fit1$estimates), c("d", "alpha_xx"))

  fit2 <- meta_fixed(yyy ~ cat, se, study, d, iter = 6000, warmup = 1000,
                     logml="s", summ="s", control=list(adapt_delta=.9))
  expect_identical(rownames(fit2$estimates), c("d", "alpha_cat1"))

  fit3 <- meta_fixed(yyy ~ xx + cat, se, study, d, iter = 6000, warmup = 1000,
                     logml="s", summ="s", control=list(adapt_delta=.9))
  expect_identical(rownames(fit3$estimates), c("d", "alpha_xx", "alpha_cat1"))
})


