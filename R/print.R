# ' Print Results of Bayesian Meta-Analysis
# '
# ' @param x a fitted meta-analysis object
# ' @param ... ignored
#' @export
print.meta_fixed <- function(x,...){
  cat("### Bayesian Fixed-Effects Meta-Analysis ###\n",
      "\n   Prior on d:   ", describe_prior(x$data$prior.d) ,"\n",
      "\n   log marginal likelihood = ", x$logmarginal,
      "\n   marginal likelihood = ", exp(x$logmarginal),
      "\n\n")
  print(x$estimates)
}


#' @export
print.meta_random <- function(x,...){
  cat("### Bayesian Random-Effects Meta-Analysis ###\n",
      "\n   Prior on d:    ", describe_prior(x$data$prior.d) ,
      "\n   Prior on tau:  ", describe_prior(x$data$prior.tau) ,"\n",
      "\n   log marginal likelihood = ", x$logmarginal,
      "\n   marginal likelihood = ", exp(x$logmarginal),
      "\n\n")
  print(x$estimates)
}


#' @export
print.meta_bma <- function(x,...){
  cat("### Bayesian Model Averaging of Meta-Analysis ###\n",
      "\n   Fixed H0:  d = 0,  tau = 0",
      "\n   Fixed H1:  d ~",describe_prior(x$meta$fixed.H1$data$prior.d)," ; tau = 0",
      "\n   Random H0: d = 0,  ",
      "\n              tau ~ ",describe_prior(x$meta$random.H0$data$prior.tau),
      "\n   Random H1: d ~",describe_prior(x$meta$random.H1$data$prior.d),
      "\n              tau ~",describe_prior(x$meta$random.H1$data$prior.tau),
      "\n\n# Bayesian Model Selection:\n")

  tab <- data.frame(prior = x$bma$prior,
                    posterior = x$bma$posterior,
                    `log marginal` = x$logmarginal,
                    marginal = exp(x$logmarginal),
                    row.names = names(x$logmarginal))
  print(tab)
  cat("\n# Posterior of d:\n")
  print(x$estimates)
}
