
bounds_prior <- function(prior, label = "par"){

  bounds <- switch(prior$name,
                   "norm" = c(-Inf, Inf),
                   "halfnorm" =  c(0, Inf),
                   "truncnorm" = prior$par[1:2],

                   "scaledt" =  c(-Inf, Inf),
                   "halft" =  c(0, Inf),

                   "cauchy" = c(-Inf, Inf),
                   "halfcauchy" =  c(0, Inf),

                   "beta" = c(0,1),
                   "shiftedbeta" = prior$par[1:2],
                   "0" = c(),
                   stop("Distribution not supported.")
  )
  if(!is.null(bounds))
    names(bounds) <- rep(label, 2)
  return(bounds)
}

# bounds_sample <- function(#pars,
#                           prior.d = NULL,
#                           prior.tau = NULL){
#
#   # set lower and upper bounds for integration
#   # lb <- rep(-Inf, ncol(samples))
#   # ub <- rep(Inf, ncol(samples))
#   # names(ub) <- names(lb) <- colnames(samples)
#   lb <- rep(-Inf, length(pars))
#   ub <- rep(Inf, length(pars))
#   names(ub) <- names(lb) <- pars
#
#   if(!missing(prior.d) && !is.null(prior.d) && prior.d$name != "0"){
#     bounds.d <- bounds_prior(prior.d, label = "d.random")
#     lb["d.random"] <- bounds.d[1]
#     ub["d.random"] <- bounds.d[2]
#   }
#   if(!missing(prior.tau) && !is.null(prior.tau)){
#     bounds.tau <- bounds_prior(prior.tau, label = "tau")
#     lb["tau"] <- bounds.tau[1]
#     ub["tau"] <- bounds.tau[2]
#   }
#
#   bounds <- list(lb=lb, ub=ub)
#   return(bounds)
# }
