library("testthat")
set.seed(1235)
SE <- runif(10, .3,1.3)
d <- data.frame(yyy = rnorm(10), SE = SE, study = 1:10,
                xx = rnorm(10), cat = c("a", "b"))

test_that("meta_fixed supports data as vector/variable names", {

  # vectors:
  fit1 <- meta_fixed(d$yyy, d$SE, d$study, rel.tol = .01)
  fit2 <- meta_fixed(d[["yyy"]], d[["SE"]], d[["study"]], rel.tol = .01)
  fit3 <- meta_fixed(d$yyy, d$SE, d$study, rel.tol = .01)
  expect_identical(fit1$logml, fit2$logml)
  expect_identical(fit1$logml, fit3$logml)
  expect_identical(fit1$estimates[,1:7,drop=FALSE], fit3$estimates[,1:7,drop=FALSE])

  # (un)quoted variable names
  fit4 <- meta_fixed("yyy", "SE", "study", d, rel.tol = .01)
  fit5 <- meta_fixed("yyy", SE, study, d, rel.tol = .01)
  fit6 <- meta_fixed(yyy, SE, study, d, rel.tol = .01)
  expect_identical(fit1$logml, fit4$logml)
  expect_identical(fit1$logml, fit5$logml)
  expect_identical(fit1$logml, fit6$logml)
  expect_identical(fit1$estimates[,1:7,drop=FALSE], fit4$estimates[,1:7,drop=FALSE])
  expect_identical(fit1$estimates[,1:7,drop=FALSE], fit5$estimates[,1:7,drop=FALSE])
  expect_identical(fit1$estimates[,1:7,drop=FALSE], fit6$estimates[,1:7,drop=FALSE])
})


test_that("meta_bma supports data as vector/variable names", {

  suppressWarnings({
    fit5  <- meta_bma(d$yyy, d$SE, d$study,    rel.tol = .01, iter = 10, logml_iter = 10, summ = "stan")
    fit7  <- meta_bma("yyy", "SE", "study", d, rel.tol = .01, iter = 10, logml_iter = 10, summ = "stan")
    fit8  <- meta_bma(yyy,   SE, study, d,     rel.tol = .01, iter = 10, logml_iter = 10, summ = "stan")
    fit9  <- meta_bma("yyy", SE, study, d,     rel.tol = .01, iter = 10, logml_iter = 10, summ = "stan")
    fit10 <- meta_bma(yyy,   SE, study, d,     rel.tol = .01, iter = 10, logml_iter = 10, summ = "stan")
  })

  # plot_posterior(fit8$meta$random, "tau")
  # hist(extract(fit8$meta$random$stanfit)[["tau"]], 300, add=T,freq=F)

  expect_equal(fit5$logml, fit8$logml, tolerance = 1e-5)
  expect_equal(fit7$logml, fit8$logml, tolerance = 1e-5)
  expect_equal(fit7$logml, fit9$logml, tolerance = 1e-5)
  expect_equal(fit7$logml, fit10$logml, tolerance = 1e-5)
})


test_that("meta_fixed supports y as formula", {
  fit  <- meta_fixed(yyy, SE, data = d, study, rel.tol = .01)
  fit2 <- meta_fixed(yyy ~ 1, SE, data = d, study, rel.tol = .01)
  expect_equal(fit$logml, fit2$logml)
})


test_that("JZS prior is correctly defined via formula", {
  expect_warning(fit1 <- meta_fixed(yyy ~ xx, SE, study, d, iter = 10, rel.tol = .1))
  expect_identical(rownames(fit1$estimates), c("d", "beta_xx"))

  expect_warning(fit2 <- meta_fixed(yyy ~ cat, SE, study, d, iter = 10, logml="s", summ="s", rel.tol = .1))
  expect_identical(rownames(fit2$estimates), c("d", "beta_cat1"))

  skip_on_cran()
  expect_warning(fit3 <- meta_fixed(yyy ~ xx + cat, SE, study, d, iter = 10, logml="s", summ="s"))
  expect_identical(rownames(fit3$estimates), c("d", "beta_xx", "beta_cat1"))
})


