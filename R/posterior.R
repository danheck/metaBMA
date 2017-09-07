
# construct posterior density function
posterior <- function (meta,
                       parameter = "d",
                       rel.tol = .Machine$double.eps^0.5){

  if (class(meta) == "meta_bma"){
    w <- meta$posterior.models
    # average across posterior densities:
    dp1 <- function (d)
      sapply(d, function (dd){
        sum(w*sapply(meta$posterior.d,
                     function(dp) dp(dd)))
      })
    lb <- sapply(meta$prior.d, attr, which = "lower")
    ub <- sapply(meta$prior.d, attr, which = "upper")
    attr(dp1, "lower") <- min(lb)
    attr(dp1, "upper") <- max(ub)
    class(dp1) <- "posterior"
    attr(dp1, "model") <- "bma"
    return (dp1)

  } else if (parameter == "d"){
    if (class(meta) == "meta_fixed"){
      dp2 <- function (d)
        post_fixed(d = d, data = meta$data, rel.tol = rel.tol)/exp(meta$logmarginal)
    } else {
      dp2 <- function (d)
        post_random_d(d = d, data = meta$data, rel.tol = rel.tol)/exp(meta$logmarginal)
    }
    attr(dp2, "lower") <- attr(meta$prior.d, "lower")
    attr(dp2, "upper") <- attr(meta$prior.d, "upper")
    class(dp2) <- "posterior"
    attr(dp2, "model") <- "fixed"
    return(dp2)

  } else if (parameter == "tau"){
    dp3 <- function (tau)
      post_random_tau(tau = tau, data = meta$data, rel.tol = rel.tol)/exp(meta$logmarginal)
    attr(dp3, "lower") <- attr(meta$prior.tau, "lower")
    attr(dp3, "upper") <- attr(meta$prior.tau, "upper")
    class(dp3) <- "posterior"
    attr(dp3, "model") <- "random"
    return (dp3)

  } else {
    dp4 <- function (tau, d)
      post_random(tau = tau, d = d, data = meta$data)/exp(meta$logmarginal)
    attr(dp4, "lower") <- c(attr(meta$prior.tau, "lower"),
                           attr(meta$prior.d, "lower"))
    attr(dp4, "upper") <- c(attr(meta$prior.tau, "upper"),
                           attr(meta$prior.d, "upper"))
    class(dp4) <- "posterior"
    attr(dp4, "model") <- "random"
    return (dp4)
  }
}
