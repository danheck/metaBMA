
bounds_prior <- function(prior, label = "par"){

  if(is.null(prior) || prior$name == "0")
    return(NULL)

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
                   stop("Distribution not supported.")
  )
  names(bounds) <- rep(label, 2)
  return(bounds)
}
