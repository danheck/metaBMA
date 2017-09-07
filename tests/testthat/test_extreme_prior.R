
set.seed(123)
y <- rnorm(20, 1,.1)
se <- runif(20, .1)

test_that("extreme priors/misspecified models still provide correct results", {
  expect_silent(meta_fixed(y, se, d.par = c(mean = 0.2, sd = .01)))
  # expect_silent(meta_random(y, se, d.par = c(mean = 0.2, sd = .05)))
})
