library(metaBMA)
library(testthat)

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


