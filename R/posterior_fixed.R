### fixed-effects
post_fixed <- function(d = 0, data, prior, log = FALSE,
                       rel.tol = .Machine$double.eps^0.5) {
  if (attr(prior, "family") == "0") {
    logprior <- 0
    d <- rep(0, length(d))
  } else {
    logprior <- prior(d, log = TRUE)
  }

  loglik <- sapply(d, function(dd) {
    sum(dnorm(data$y, mean = dd, sd = data$SE, log = TRUE))
  })

  post <- logprior + loglik
  if (!log) {
    post <- exp(post)
  }
  post
}

loglik_fixed_H0 <- function(data) {
  sum(dnorm(data$y, mean = 0, sd = data$SE, log = TRUE))
}

post_fixed_norm <- function(d, data, prior, log = FALSE,
                            rel.tol = .Machine$double.eps^0.5) {
  bounds <- bounds_prior(prior)
  if (diff(bounds) != 0) {
    const <- integrate(post_fixed,
      data = data, prior = prior, rel.tol = rel.tol,
      lower = bounds[1], upper = bounds[2]
    )$value
  } else {
    const <- 1
  }
  post <- post_fixed(d, data, prior) / const
  if (log) {
    post <- log(post)
  }
  post
}
