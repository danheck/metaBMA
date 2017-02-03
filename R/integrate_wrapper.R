integrate_wrapper <- function (data,
                               rel.tol = .Machine$double.eps^0.35){

  integral <- NULL

  ####### fixed H1
  if (data$model == "fixed" && attr(data$prior.d, "family") != "0"){
    try (integral <- integrate(post_fixed, data = data,
                               attr(data$prior.d, "lower"),
                               attr(data$prior.d, "upper"),
                               rel.tol = rel.tol))

    ####### random H0
  } else if (data$model == "random" && attr(data$prior.d, "family") == "0"){
    try (integral <- integrate(post_random, d = 0, data = data,
                               attr(data$prior.tau, "lower"),
                               attr(data$prior.tau, "upper"),
                               rel.tol = rel.tol))
  } else {
    ####### random H1

    try (integral <- integrate(post_random_d, data=data,
                               attr(data$prior.d, "lower"),
                               attr(data$prior.d, "upper"),
                               rel.tol = rel.tol))
    # try (i2 <- integrate(post_random_tau, data=data,
    #                      attr(data$prior.tau, "lower"),
    #                      attr(data$prior.tau, "upper"),
    #                      rel.tol = .Machine$double.eps^0.35))
    #
    # if (is.null(i1) || is.null(i2)){
    #   stop ("Marginal likelihood could not be computed with ?integrate")
    # } else if ( max(i1$abs.error, i2$abs.error) / i1$value > .01){
    #   warning ("Order of integration resulted in different estimates!\n    ",
    #           i1$value, "  vs.  ", i2$value)
    # }
    # if (i1$abs.error < i2$abs.error){
    #   integral <- i1
    # } else {
    #   integral <- i2
    # }
  }
  if (is.null(integral))
    warning ("Integral for marginal likelihood could not be computed with ?integrate")

  try (integral$logml <- log(integral$value))
  return (integral)
}
