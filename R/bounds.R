
bounds_prior <- function (family,
                          param,
                          label = "d",
                          lower = -Inf,
                          upper = Inf){

  if (is.null(family))
    return (NULL)

  if (class(family) == "prior"){
    param <- attr(family, "param")
    label <- attr(family, "label")
    lower <- attr(family, "lower")
    upper <- attr(family, "upper")
    family <- attr(family, "family")
  }

  bounds <- switch (family,
                    "norm" = c(-Inf, Inf),
                    "halfnorm" = c(0, Inf),
                    "truncnorm" = param[1:2],

                    "scaledt" = c(-Inf, Inf),
                    "halft" = c(0, Inf),

                    "cauchy" = c(-Inf, Inf),
                    "halfcauchy" =  c(0, Inf),

                    "triangular" = param[c(1,3)],
                    "beta" = c(0,1),
                    "shiftedbeta" = param[1:2],

                    "0" = c(0, 0),
                    "custom" = c(lower, upper),

                    stop ("Prior distribution not supported. See ?metaBMA::prior")
  )
  names(bounds) <- rep(label, 2)
  return (bounds)
}
