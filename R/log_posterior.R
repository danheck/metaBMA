
# d, tau: numeric vector
# data: list with y, V, prior.d, prior.tau

### fixed-effects
post_fixed <- function(d = 0,
                       data,
                       log = FALSE){

  if(data$prior.d$name == "0"){
    prior <- 0
    d <- rep(0, length(d))
  }else{
    log_prior_d <- log_prior(data$prior.d)
    prior <- log_prior_d(d)
  }

  loglik <-  sapply(d, function(dd)
    sum(dnorm(data$y, mean = dd, sd = sqrt(data$V), log = TRUE)))

  post <- prior + loglik
  if(!log) post <- exp(post)
  return(post)
}

post_fixed_norm <- function(d,
                            data){
  bounds <- bounds_prior(data$prior.d)
  const <- integrate(post_fixed, data=data)$value
  post_fixed(d)/const
}


### random-effects: marginal posterior of both
post_random <- function(tau,
                        d = 0,
                        data,
                        log = FALSE){

  ### priors
  log_prior_tau <- log_prior(data$prior.tau)
  prior <-  log_prior_tau(tau)

  if(data$prior.d$name != "0"){
    log_prior_d <- log_prior(data$prior.d)
    prior <- prior + log_prior_d(d)
  }else{
    d <- 0
  }

  ### loglikelihood
  pars <- cbind(d, tau)
  ll <- function(x) sum(dnorm(data$y, mean = x[1],
                              sd = sqrt(data$V + x[2]^2), log = TRUE))
  loglik <- apply(pars, 1, ll)

  post <- prior + loglik
  if(!log) post <- exp(post)
  return(post)
}


post_random_d <- function(d,
                          data,
                          log = FALSE){
  bounds <- bounds_prior(data$prior.tau)
  func <- function(d)
    integrate(post_random, d = d, data = data,
              bounds[1], bounds[2],
              rel.tol = .Machine$double.eps^0.35)$value

  sapply(d, func)
}


post_random_tau <- function(tau,
                            data,
                            log = FALSE){

  if(data$prior.d$name != "0"){
    bounds <- bounds_prior(data$prior.d)
    func <- function(tau) integrate( function(x)
      post_random(tau = tau, d = x, data = data),
      lower = bounds[1],  upper = bounds[2],
      rel.tol = .Machine$double.eps^0.35)$value
  }else{
    func <- function(tau) post_random(tau = tau, d = 0, data = data)
  }

  post <- sapply(tau, func)
  if(log) post <- log(post)
  return(post)
}

