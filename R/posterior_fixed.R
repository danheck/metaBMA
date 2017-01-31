

### fixed-effects
post_fixed <- function(d = 0,
                       data,
                       log = FALSE){

  if(data$prior.d$name == "0"){
    prior <- 0
    d <- rep(0, length(d))
  }else{
    log_prior_d <- prior(data$prior.d, log = TRUE)
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


