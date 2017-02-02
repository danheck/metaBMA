
# Test prior functions


test_that('prior returns vectorized function', {

  priors <- c("norm", "halfnorm", "cauchy", "halfcauchy","truncnorm",
              "scaledt", "halft", "triangular", "beta")
  params <- list(c(0,.3), c(0,.3), .5, .5, c(1,2,0,3),
                 c(0, .2, 2), c(.2, 2), c(0,1,2),c(1,1))
  for(i in seq_along(priors)){
    pp <- prior(priors[i], params[[i]], "xx")
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
