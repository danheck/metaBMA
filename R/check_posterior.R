
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
    dpost2 <- stan_logspline(meta$stanfit, parameter,
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

stan_logspline <- function (stanfit, parameter, prior){
  if (missing(stanfit) || is.null(stanfit))
    warning ("MCMC/Stan samples missing: To approximate the posterior density",
             "\n  by MCMC samples, one of the available priors must be used (see ?prior)",
             "\n and the argument 'sample' must be larger than zero!")
  if (class(stanfit) == "stanfit")
    ss <- extract(stanfit, parameter)[[parameter]]
  else
    ss <- stanfit  # samples
  if (!is.null(ss)){
    bnd <- bounds_prior(prior)
    mini <- max(-3, bnd[1])
    maxi <- min(3, bnd[2])
    lspline <- logspline(ss, min(ss, mini), max(ss, maxi))
    dens <- function(x) dlogspline(x, lspline)
    # if (log) dx <- log(dx)
  } else {
    stop("parameter '", parameter, "' not included in stanfit object.")
  }
  dens
}
