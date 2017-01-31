
#################### for bridge sampling: requires list s.row with samples!

# s.row: list with matrices of samples
# data: list with y, V, prior.d, ....
log_posterior <- function(s.row ,
                          data){

  if(data$prior.d$name == "0"){
    switch(data$model,
           "fixed" = {
             lp <- loglik_fixed_H0(data)
           },
           "random" = {
             tau <- s.row[[ "tau" ]]
             log_prior_tau <- prior(data$prior.tau, log = TRUE)

             lp <-  loglik_random(data, s.row) + log_prior_tau(tau)
           })
  }else{
    switch(data$model,
           "fixed" = {
             d.fixed <- s.row[[ "d.fixed" ]]
             log_prior_d <- prior(data$prior.d, log = TRUE)
             lp <- loglik_fixed(data, s.row) + log_prior_d(d.fixed)
           },
           "random" = {
             d.random <- s.row[[ "d.random" ]]
             tau <- s.row[[ "tau" ]]
             log_prior_tau <- prior(data$prior.tau, log = TRUE)
             log_prior_d <- prior(data$prior.d, log = TRUE)

             lp <-  loglik_random(data, s.row) +
               log_prior_tau(tau) + log_prior_d(d.random)
           })
  }
  return(lp)
}

loglik_fixed_H0 <- function(data) {
  sum(dnorm(data$y, mean = 0, sd = sqrt(data$V), log = TRUE))
}



loglik_fixed <- function(data,
                         s.row){

  d.fixed <- s.row[[ "d.fixed" ]]
  sum(dnorm(data$y, mean = d.fixed,
            sd = sqrt(data$V), log = TRUE))
}


# marginal (!) loglikelihood => random effects integrated out!
loglik_random <- function(data,
                          s.row) {

  if(data$prior.d$name == "0")
    d.random <- 0
  else
    d.random <- s.row[[ "d.random" ]]
  tau <- s.row[[ "tau" ]]

  ####### not marginalized:
  # d <- s.row[ paste0("d[", seq_along(data$y), "]") ]
  # sum(dnorm(data$y, mean = d, sd = sqrt(data$V), log = TRUE)) +
  #   sum(dnorm(d, mean = d.random, sd = tau, log = TRUE))

  sum(dnorm(data$y, mean = d.random,
            sd = sqrt(data$V + tau^2), log = TRUE))
}
