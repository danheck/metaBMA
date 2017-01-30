
jags_prior <- function(prior, label = NULL){
  par <- prior$par
  jags <- switch(prior$name,
                 "norm" = paste0("dnorm(",par[1],",", 1/par[2]^2, ")"),
                 "halfnorm" =  paste0("dnorm(",par[1],",", 1/par[2]^2, ")T(0,)"),
                 "truncnorm" = paste0("dnorm(",par[3],",", 1/par[4]^2,
                                      ")T(",par[1],",",par[2],")"),

                 "scaledt" =  paste0("dt(", par[1], ", 1/",par[2],"^2, ", par[3], ")"),
                 "halft" =  paste0("dt(0, 1/",par[1],"^2, ", par[2], ")T(0,)"),

                 "cauchy" =
                   paste0("dnorm(0,scale_",label,")\n  ",
                          "scale_",label," ~ dgamma(1/2, ",par[1],"^2/2)  "),
                 "halfcauchy" =
                   paste0("dnorm(0,scale_",label,")T(0,)\n  ",
                          "scale_",label," ~ dgamma(1/2, ",par[1],"^2/2)  "),

                 "beta" = paste0("dbeta(",par[1], ",", par[2],")"),
                 "shiftedbeta" =
                   "NULL - SHIFTED BETA NOT YET SUPPORTED (problem: shifting requires new parameter)",
                 "0" = c(NA, NA),
                 stop("Distribution not supported.")
  )

  return(jags)
}


describe_prior <- function(prior){
  par <- prior$par
  labels <- switch(prior$name,
                   "norm" = paste0("mean=",par[1], ", sd=",par[2]),
                   "halfnorm" =  paste0("mean=",par[1], ", sd=",par[2]),
                   "truncnorm" = paste0("min=",par[1], ", max=",par[2],
                                        "mean=",par[3], ", sd=",par[4]),

                   "scaledt" =  paste0("mean=", par[1], ", sigma=",par[2],", nu=", par[3]),
                   "halft" =  paste0("scale=",par[1],", nu=", par[2]),

                   "cauchy" = paste0( "scale=",par[1]),
                   "halfcauchy" =   paste0( "scale=",par[1]),

                   "beta" =  paste0( "alpha=",par[1], ", beta=",par[2]),
                   "shiftedbeta" =paste0( "min=",par[1], ", max=",par[2],
                                          ", alpha=",par[3], ", beta=",par[4]),
                   "0" = "H0 (parameter = 0)",
                   stop("Distribution not supported.")
  )
  paste0("'", prior$name, "' with ", labels)
}


# prior_dens <- function(prior, log = FALSE){
#   dens <- log_prior(pior)
#   function(x) dens(x, )
# }
