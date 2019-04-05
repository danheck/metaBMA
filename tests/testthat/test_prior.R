library(metaBMA)
library(testthat)

set.seed(123)
priors <- c("norm", "t", "invgamma", "beta")
params <- list("norm" = c(mean = 0, sd = .3),
               "t" = c(location = 0, scale = .3, nu = 1),
               "invgamma" = c(shape = 1, scale = 1),
               "beta" = c(1, 2))

test_that('prior returns vectorized function', {

  for(i in seq_along(priors)){
    for (lower in c(-Inf, 0)){
      for (upper in c(1, Inf)){
        if (! (priors[i] == "beta" & (lower==-Inf || upper == Inf))){
          pp <- prior(priors[i], params[[i]], lower = lower, upper = upper)
          plot(pp)
          plot(pp, from = -1, to = 1.5)
          expect_s3_class(pp, "prior")
          expect_true(is.function(pp))
          expect_length(pp(1:10), 10)

          expect_equal(pp(1:3, log = TRUE), log(pp(1:3)))
          aa <- attributes(pp)
          expect_true(all(c("family", "param", "label",
                            "lower", "upper") %in% names(aa)))
          expect_is(aa$lower, "numeric")
          expect_is(aa$upper, "numeric")
          expect_lt(aa$lower, aa$upper)

          # random number generation
          expect_silent(x <- metaBMA:::rprior(3, pp))
          expect_length(x, 3)
        }
      }
    }
  }

})


test_that('prior crashes for non-vectorized/negative functions', {

  # wrong number of arguments
  expect_error(prior("norm", c(0), "xx"))
  expect_error(prior("cauchy", NULL, "xx"))

  expect_error(prior("custom", 1, "xx"))
  expect_error(prior("custom", function(x) x, "xx"))
  expect_error(prior("custom", function(x) -dunif(x), "xx"))

})


test_that('expected value in truncnorm_mean() is correct', {
  x <- metaBMA:::rtrunc(5e5, "norm", 0, Inf, mean=.14, sd=.5)
  expect_silent(avg <- truncnorm_mean(.14, .5, 0, Inf))
  expect_equal(mean(x), avg, tolerance = .0001)
  expect_length(truncnorm_mean(0:1, c(.3,.4), 0, 4), 2)
})


test_that("log_diff_exp() correctly implemented", {
  x <- runif(2, -10,-1)
  expect_silent(lde <- metaBMA:::log_diff_exp(x[1], x[2]))
  expect_equal(lde, log(exp(x)[1] - exp(x)[2]))
})
