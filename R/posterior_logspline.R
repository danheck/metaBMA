
posterior_logspline <- function(stanfit, parameter, prior){

  if (missing(stanfit) || is.null(stanfit))
    warning ("MCMC/Stan samples missing: To approximate the posterior density",
             "\n  by MCMC samples, one of the available priors must be used (see ?prior)",
             "\n and the argument 'sample' must be larger than zero!")

  if (class(stanfit) == "stanfit"){
    ss <- extract(stanfit, parameter)[[parameter]]
  } else {
    ss <- stanfit
  }

  bnd <- bounds_prior(prior)
  mini <- max(-Inf, bnd[1])
  maxi <- min(Inf, bnd[2])

  args <- list("x" = ss,
               "knots" = quantile(ss, probs = c(.20,.50, .80)),
               "maxknots" = 5)
  if (mini != -Inf) args$lbound <- mini
  if (maxi != Inf) args$ubound <- maxi

  lspline <- do.call("logspline", args)
  dens <- function(x) dlogspline(x, lspline)
  dens
}
