
test_that("priors for stan models are properly checked", {

  set.seed(123452)
  d <- data.frame(yyy = rnorm(10), se = runif(10), xx = rnorm(10), study = 1:10)

  expect_error(meta_stan(yyy, se, data = d,
                         tau = c(1e5, 0, 100, -1, Inf)))
  expect_error(meta_stan(yyy, se, data = d,
                         d = c(1e5, 0, 100, -Inf, NA)))
  expect_error(meta_stan(yyy, se, data = d,
                         d = c(1e5, 0, 100, 4, 1)))
  expect_silent(meta_stan(yyy, se, data = d,
                         d = c(1e5, 0, 100, -3,3),
                         tau = c(1e5, 0, 100, 0, 3), iter = 50))
})
