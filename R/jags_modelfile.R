
jags_model <- function(data){

  jags_ll <- "model
{
  for (i in 1:length(y))
  {
    P[i] <- 1/SE[i]^2"

  ######################################## fixed effects model

  if(data$model == "fixed"){
    prior <- paste0("  d.fixed ", jags_prior(data$prior.d))

    jags_complete <- paste0(jags_ll,
                            "\n    y[i] ~ dnorm(d.fixed, P[i])\n  }\n",
                            prior, "\n}")

    ######################################## fixed effects model

  }else if(data$model == "random"){
    prior.d <- paste0("  d.random ",
                      jags_prior(data$prior.d, label="d.random"))
    prior.tau <- paste0("\n  tau ",
                        jags_prior(data$prior.tau, label="tau"))

    jags_complete <- paste0(jags_ll,
                            "\n      d[i]~ dnorm(d.random, prec) # random effect",
                            "\n      y[i] ~ dnorm(d[i], P[i])\n    }",
                            "\n  prec <- pow(tau, -2)\n",
                            prior.d,
                            prior.tau, "\n}")
  }

  jagsfile <- tempfile(pattern = "metaBMA_",fileext = ".txt")
  cat(jags_complete, file = jagsfile)

  return(list("model" = jags_complete,
              "file" = jagsfile))
}



