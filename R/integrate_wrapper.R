integrate_wrapper <- function (samples,
                               data
                               # method = "bridge"
){

  ####### fixed H1
  if (data$model == "fixed" && attr(data$prior.d, "family") != "0"){
    integral <- integrate(post_fixed, data = data,
                          attr(data$prior.d, "lower"),
                          attr(data$prior.d, "upper"),
                          rel.tol = .Machine$double.eps^0.3)

    ####### random H0
  } else if (data$model == "random" && attr(data$prior.d, "family") == "0"){
    integral <- integrate(post_random, d = 0, data=data,
                          attr(data$prior.tau, "lower"),
                          attr(data$prior.tau, "upper"),
                          rel.tol = .Machine$double.eps^0.3)
  } else {
    ####### random H1

    tryCatch (
      i1 <- integrate(post_random_d, data=data,
                      attr(data$prior.d, "lower"),
                      attr(data$prior.d, "upper"),
                      rel.tol = .Machine$double.eps^0.3),
      error = i1 <- NULL)
    tryCatch (
      i2 <- integrate(post_random_tau, data=data,
                      attr(data$prior.tau, "lower"),
                      attr(data$prior.tau, "upper"),
                      rel.tol = .Machine$double.eps^0.3),
      error = i2 <- NULL)

    if (is.null(i1) || is.null(i2)){
      stop ("Marginal likelihood could not be computed with marginal='integrate'.\n",
            "  Consider switching to marginal='bridge'.")
    } else if ( max(i1$abs.error, i2$abs.error) / i1$value > .01){
      warning("Order of integration resulted in different estimates!\n    ",
              i1$value, "  vs.  ", i2$value)
    }
    if (i1$abs.error < i2$abs.error){
      integral <- i1
    } else {
      integral <- i2
    }
  }

  integral$logml <- log(integral$value)

  return (integral)
}
