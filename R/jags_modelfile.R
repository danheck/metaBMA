
jags_model <- function(data){

  model <- data$model

  jags_ll <- "model
{
  for (i in 1:length(y))
  {
    P[i] <- 1/V[i]"

  ######################################## fixed effects model

  if(model == "fixed"){

    if(data$prior.d$name == "0")
      prior <- "  d.fixed <- 0"
    else
      prior <- paste0("  d.fixed ~ ", jags_prior(data$prior.d, label="dfix"))

    jags_complete <- paste0(jags_ll,
                            "\n    y[i] ~ dnorm(d.fixed, P[i])\n  }\n",
                            prior, "\n}")

    ######################################## fixed effects model

  }else if(model == "random"){

    if(data$prior.d$name == "0")
      prior.d <- "  d.random <- 0"
    else
      prior.d <- paste0("  d.random ~ ", jags_prior(data$prior.d, label="drand"))

    prior.tau <- paste0("\n  tau ~ ", jags_prior(data$prior.tau, label="tau"))

    jags_complete <- paste0(jags_ll,
                            "\n      d[i]~ dnorm(d.random, prec) # random effect",
                            "\n      y[i] ~ dnorm(d[i], P[i])\n    }",
                            "\n  prec <- pow(tau, -2)\n",
                            prior.d, prior.tau, "\n}")
  }

  jagsfile <- tempfile(pattern = "metama_",fileext = ".txt")
  cat(jags_complete, file = jagsfile)

  return(list(model = jags_complete,
              file = jagsfile))
}



