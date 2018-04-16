set.seed(123452)
se <- runif(10, .3,1.3)
d <- data.frame(yyy = rnorm(10), se = se, xx = rnorm(10), study = 1:10)
data_list <- list(model = "random", N = nrow(d), y = d$yyy, se = d$se, labels = d$study,
                  data = NULL, model.frame = NULL)


test_that("priors for stan models are properly checked", {

  expect_output(ms <- metaBMA:::meta_stan(data_list, iter = 1000))
  expect_true(class(ms) == "stanfit")
  ms <- meta_random(yyy, se, data = d, iter = 100)
  expect_true(class(ms) == "meta_random")
  expect_true(class(ms$stanfit) == "stanfit")
  expect_error(metaBMA:::meta_stan(data_list, d = prior("dsad"), iter = 100))
  expect_error(metaBMA:::meta_stan(data_list, prior("t", c(1,0,0)), iter = 100))
  expect_error(metaBMA:::meta_stan(data_list, prior("t", c(1,3,-10)), iter = 100))
})

