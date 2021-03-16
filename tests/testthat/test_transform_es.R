library(metaBMA)
library(testthat)

set.seed(123)
y <- runif(5, -.8, .8)   # also works for correlations r
SE <- runif(5, .1, .5)
es <- c("d", "logOR", "r", "z")

test_that("transform_es provides consistent transformations", {

  for (i in seq_along(es)) {
    for (j in seq_along(es)) {
      expect_silent(X <- transform_es(y, SE, from = es[i], to = es[j]))
      expect_length(X, 10)
      expect_type(X, "double")

      expect_silent(Y_reverse <- transform_es(X[,1], X[,2], from = es[j], to = es[i]))
      Y <- cbind(y, SE)
      colnames(Y) <- c(es[i], "SE")
      expect_equal(Y, Y_reverse)
    }
  }

})

test_that("transform_es returns expected output", {

  expect_silent(d <- transform_es(.5, from = "logOR", to ="d"))
  expect_length(d, 1)
  expect_vector(d, c(1), 1)

  expect_silent(d <- transform_es(c(.3,.5), from = "logOR", to ="d"))
  expect_length(d, 2)
  expect_vector(d, c(1,2), 2)

})

test_that("transform_es crashes for inappropriate arguments", {

  # wrong number of arguments
  expect_error(transform_es(.5))
  expect_error(transform_es(.5, from ="csasdasd", to = "d"))
  expect_error(transform_es(.5, from = "d", to ="csasdasd"))
  expect_error(transform_es(from = "logOR", to ="d"))

  # inadmissible arguments
  expect_error(transform_es(-1.4, from = "r", to ="d"))
  expect_error(transform_es(1.4, from = "r", to ="d"))
  expect_error(transform_es(.5, -.3, from = "r", to ="d"))

})

