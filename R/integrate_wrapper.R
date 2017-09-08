integrate_wrapper <- function (data,
                               rel.tol = .Machine$double.eps^0.5){

  scale <- 1
  integral <- NA

  ####### fixed H1
  if (data$model == "fixed" && attr(data$prior.d, "family") != "0"){
    try(scale <- integrate(post_fixed, data = data,
                           attr(data$prior.d, "lower"),
                           attr(data$prior.d, "upper"),
                           rel.tol = rel.tol)$value)
    try (integral <- integrate(function(x) post_fixed(x, data = data)/scale,
                               attr(data$prior.d, "lower"),
                               attr(data$prior.d, "upper"),
                               rel.tol = rel.tol)$value)

    ####### random H0
  } else if (data$model == "random" && attr(data$prior.d, "family") == "0"){
    try(scale <- integrate(post_random, d = 0, data = data,
                           attr(data$prior.tau, "lower"),
                           attr(data$prior.tau, "upper"),
                           rel.tol = rel.tol)$value)
    try (integral <- integrate(function(x) post_random(x, d = 0, data = data)/scale,
                               attr(data$prior.tau, "lower"),
                               attr(data$prior.tau, "upper"),
                               rel.tol = rel.tol)$value)
  } else {
    ####### random H1
    try ({
      scale <- integrate(function(x) post_random_d(x, data=data, rel.tol = rel.tol),
                         attr(data$prior.d, "lower"),
                         attr(data$prior.d, "upper"),
                         rel.tol = rel.tol)$value
      integral <- integrate(function(x) post_random_d(x, data=data, rel.tol = rel.tol)/scale,
                            attr(data$prior.d, "lower"),
                            attr(data$prior.d, "upper"),
                            rel.tol = rel.tol)$value
      # i1 <- scale*integral
      # test1 <-  integrate(function(x) post_random_tau(x, data=data, rel.tol = rel.tol)/scale/integral,
      #                     attr(data$prior.tau, "lower"),
      #                     attr(data$prior.tau, "upper"),
      #                     rel.tol = rel.tol)$value
      # if (test1)
    })
    # try({
    #   scale <- integrate(function(x) post_random_tau(x, data=data, rel.tol = rel.tol),
    #                      attr(data$prior.tau, "lower"),
    #                      attr(data$prior.tau, "upper"),
    #                      rel.tol = rel.tol)$value
    #   integral <- integrate(function(x) post_random_tau(x, data=data, rel.tol = rel.tol)/scale,
    #                         attr(data$prior.tau, "lower"),
    #                         attr(data$prior.tau, "upper"),
    #                         rel.tol = rel.tol)$value
    #   i2 <- scale*integral
    #   test2 <- integrate(function(x) post_random_d(x, data=data, rel.tol = rel.tol)/scale/integral,
    #                      attr(data$prior.d, "lower"),
    #                      attr(data$prior.d, "upper"),
    #                      rel.tol = rel.tol)$value
    # })
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
  if (is.na(integral))
    warning ("Integral for marginal likelihood could not be computed with ?integrate")

  # try (integral$value <- integral$value*scale)
  # try (integral$abs.error <- NULL)
  # try (class(integral) <- NULL)
  # try (integral$logml <- log(integral$value))
  log(integral * scale)
}
