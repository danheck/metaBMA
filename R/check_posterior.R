
check_posterior <- function (dpost, meta, parameter = "d",
                             rel.tol = .Machine$double.eps^0.3, abs.tol = .001){

  bnd <- bounds_prior(dpost)
  mini <- max(-3, bnd[1])
  maxi <- min(3, bnd[2])
  dx <- dpost(seq(mini, maxi, length.out = 11))
  dp_const <- 1
  dpost2 <- dpost

  if (any(is.na(dx)) || is.na(dp_const)){
    warning ("Posterior distribution could not be approximated numerically\n",
             "  (posterior density integrates to: ", dp_const, ")\n",
             "  A logspline density approximation to the MCMC/Stan samples is used instead.")
    dpost2 <- posterior_logspline(meta$stanfit, parameter,
                                  prior = meta[[paste0("prior_", parameter)]])
  }

  if (bnd[1] < bnd[2])
    try(dp_const <- integrate(dpost2, lower = bnd[1], upper = bnd[2],
                              rel.tol = rel.tol, subdivisions = 1000L)$value,
        silent = TRUE)
  if (abs(dp_const - 1) > abs.tol){
    dpost2 <- function(x) dpost(x) / dp_const
  }
  class(dpost2) <- "posterior"
  attr(dpost2, "lower") <- attr(dpost, "lower")
  attr(dpost2, "upper") <- attr(dpost, "upper")
  attr(dpost2, "model") <- attr(dpost, "model")
  attr(dpost2, "parameter") <- parameter
  dpost2
}


