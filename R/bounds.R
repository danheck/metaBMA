default_lower <- function(family){
  switch(family,
         "norm" = -Inf,
         "t" = -Inf,
         "invgamma" = 0,
         "beta" = 0,
         "0" = 0,
         "custom" = stop("'lower' needs to be defined for 'custom' priors."),
         NA)
}
default_upper <- function(family){
  switch(family,
         "norm" = Inf,
         "t" = Inf,
         "invgamma" = Inf,
         "beta" = 1,
         "0" = 0,
         "custom" = stop("'upper' needs to be defined for 'custom' priors."),
         NA)
}

bounds_prior <- function(prior){
  c(attr(prior, "lower"), attr(prior, "upper"))
}

# bounds_prior <- function (family, param, lower = -Inf, upper = Inf, label = "d"){
#   stopifnot(length(lower) == 1)
#   stopifnot(length(upper) == 1)
#   stopifnot(lower <= upper)
#
#   if (is.null(family))
#     return (NULL)
#
#   if (class(family) == "prior"){
#     family <- attr(family, "family")
#     param <- attr(family, "param")
#     lower <- attr(family, "lower")
#     upper <- attr(family, "upper")
#   }
#
#   bounds <- switch (family,
#                     "0" = c(0, 0),
#                     c(lower, upper))
#   #                   "norm" = c(max(lower, -Inf), min(upper, Inf)),
#   #                   "t" = c(max(lower, -Inf), min(upper, Inf)),
#   #                   "invgamma" = c(max(lower, 0), min(upper, Inf)),
#   #                   "beta" = c(lower, upper),
#                     # "triangular" = param[c(1,3)],
#
#                     # stop ("Prior distribution not supported. See ?metaBMA::prior")
#   names(bounds) <- rep(label, 2)
#   bounds
# }
