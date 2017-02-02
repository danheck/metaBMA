
### random-effects: marginal posterior of both
post_random <- function(tau,
                        d = 0,
                        data,
                        log = FALSE){

  ### priors
  prior <- data$prior.tau(tau, log = TRUE)

  if (attr(data$prior.d, "family") != "0"){
    prior <- prior + data$prior.d(d, log = TRUE)
  }else{
    d <- 0
  }

  ### loglikelihood
  pars <- cbind(d, tau)
  ll <- function(x) sum(dnorm(data$y, mean = x[1],
                              sd = sqrt(data$SE^2 + x[2]^2), log = TRUE))
  loglik <- apply(pars, 1, ll)

  post <- prior + loglik
  if (!log) post <- exp(post)
  return(post)
}

################################## integration over 1 parameter ######

post_random_d <- function(d,
                          data,
                          log = FALSE){
  bounds <- bounds_prior(data$prior.tau)
  func <- function(d)
    integrate(post_random, d = d, data = data,
              bounds[1], bounds[2],
              rel.tol = .Machine$double.eps^0.3)$value

  sapply(d, func)
}


post_random_tau <- function(tau,
                            data,
                            log = FALSE){

  if (attr(data$prior.d, "family") != "0"){
    bounds <- bounds_prior(data$prior.d)
    func <- function(tau) integrate( function(x)
      post_random(tau = tau, d = x, data = data),
      lower = bounds[1],  upper = bounds[2],
      rel.tol = .Machine$double.eps^0.3)$value
  }else{
    func <- function(tau) post_random(tau = tau, d = 0, data = data)
  }

  post <- sapply(tau, func)
  if (log) post <- log(post)
  return(post)
}


post_random_theta <- function(theta,
                              idx,
                              data,
                              log = FALSE){

  bd <- bounds_prior(data$prior.d)
  bt <- bounds_prior(data$prior.tau)

  f.d.tau <- function(d, theta, tau)
    sapply(d, function(dd)
      dnorm(theta,
            mean = (1/tau^2*dd + 1/data$SE[idx]^2 * data$y[idx])/(1/tau^2 + 1/data$SE[idx]^2),
            sd = 1/sqrt(1/tau^2 + 1/data$SE[idx]^2)) *
        data$prior.d(dd) *
        data$prior.tau(1/tau^2))

  f.tau <- function(tau, theta)
    sapply(tau, function(tt)
      integrate(f.d.tau, bd[1], bd[2], theta = theta, tau = tt)$value)
  post <- sapply(theta, function(tt)
    integrate(f.tau, bt[1], bt[2], theta = tt,
              rel.tol = .Machine$double.eps^0.25)$value)

  # f.tau.d <- function(tau, theta, d)
  #   sapply(tau, function(tt)
  #     dnorm(theta,
  #           mean = (tt*d + 1/data$V[idx] * data$y[idx])/(tt + 1/data$V[idx]),
  #           sd = 1/sqrt(tt + 1/data$V[idx]))*
  #       # dnorm(data$y[idx],
  #       #       mean = theta,
  #       #       sd = sqrt(data$V[idx])) *
  #       #   dnorm(theta,
  #       #         mean = d,
  #       #         sd = 1/sqrt(tt)) *
  #       data$prior.d(d) *
  #       data$prior.tau(tt))
  #
  # f.d <- function(d, theta)
  #   sapply(d, function(dd)
  #     integrate(f.tau.d, bt[1], bt[2], theta = theta,
  #               d = dd, rel.tol = .Machine$double.eps^0.2)$value)
  #
  # post <- sapply(theta, function(tt)
  #   integrate(f.d, bd[1], bd[2], theta = tt,
  #             rel.tol = .Machine$double.eps^0.25)$value)

  if (!log) post <- exp(post)
  return(post)
}

# debug(metaBMA:::post_random_theta)
# metaBMA:::post_random_theta(.3, 1, mr$data)
# library(metaBMA)
# idx <- 1
# mm <- mr$data$y[idx]
# ss <- sqrt(mr$data$V[idx])
# x <- 1
# curve( metaBMA:::post_random_theta(x, idx, mr$data)-1, n = 31, -1,1.5)#mm-3*ss, mm+3*ss)
# metaBMA:::post_random_theta(-1, idx, mr$data)
#
#
############ check conjugacy for study-effect parameters
# theta <- .
# tt <- c(.1,.3,.5)
# V <- .2
# y <- .4
# d <- .45
# tau <- .2
# curve(dnorm(x,
#             mean = (tau*d + 1/V * y)/(tau + 1/V),
#             sd = 1/sqrt(tau + 1/V)),-2,3)
# curve(5.7*dnorm(y,
#                 mean = x,
#                 sd = sqrt(V)) *
#         dnorm(x,
#               mean = d,
#               sd = 1/sqrt(tau)), add=T, col=2)
