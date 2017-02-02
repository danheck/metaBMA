

### fixed-effects
post_fixed <- function (d = 0,
                        data,
                        log = FALSE){

  if (attr(data$prior.d, "family") == "0"){
    prior <- 0
    d <- rep(0, length(d))
  } else {
    prior <- data$prior.d(d, log = TRUE)
  }

  loglik <-  sapply(d, function (dd)
    sum(dnorm(data$y, mean = dd, sd = data$SE, log = TRUE)))

  post <- prior + loglik
  if (!log)
    post <- exp(post)
  return (post)
}

loglik_fixed_H0 <- function(data) {
  sum(dnorm(data$y, mean = 0, sd = data$SE, log = TRUE))
}

post_fixed_norm <- function (d,
                             data,
                             log = FALSE){
  bounds <- bounds_prior(data$prior.d)
  const <- integrate(post_fixed, data=data)$value
  post <- post_fixed(d)/const
  if(log)
    post <- log(post)
  return(post)
}


