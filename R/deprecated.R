check_deprecated <- function(dots) {
  if ("d.par" %in% names(dots)) {
    stop(
      "The argument 'd.par' is deprecated for metaBMA (>0.3.9).",
      "\n  The prior distribution on effect size is now defined via:",
      "\n    d = prior(family='norm', param = c(mean=0, sd=1))",
      "\n  (see ?prior)."
    )
  }

  if ("tau.par" %in% names(dots)) {
    stop(
      "The argument 'tau.par' is deprecated for metaBMA (>0.3.9).",
      "\n  The prior distribution on heterogeneity is now defined via:",
      "\n    tau = prior(family='invgamma', param = c(shape = 1, scale = 0.15))",
      "\n  (see ?prior)"
    )
  }

  if ("sample" %in% names(dots)) {
    stop(
      "The argument 'sample' is deprecated for metaBMA (>0.3.9).",
      "\n  MCMC settings (for Stan instead of JAGS) can be changed via:",
      "\n    iter = 10000, chains = 6, cores = 4",
      "\n  (see ?rstan::rstan)"
    )
  }
}
