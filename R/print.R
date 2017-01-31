# ' Print Results of Bayesian Meta-Analysis
# '
# ' @param x a fitted meta-analysis object
# ' @param ... ignored
#' @export
print.meta_fixed <- function(x,...){
  cat("### Bayesian Fixed-Effects Meta-Analysis ###\n",
      "\n   Prior on d:   ", describe_prior(x$data$prior.d) ,
      # "\n   marginal likelihood (H1)        =", exp(x$logmarginal),
      # "\n   marginal likelihood (H0)        =", exp(x$logmarginal.H0),
      "\n   Bayes factor (d ~ prior vs. d = 0) =", x$BF_10,
      "\n\n")
  print(x$estimates)
}


#' @export
print.meta_random <- function(x,...){
  cat("### Bayesian Random-Effects Meta-Analysis ###\n",
      "\n   Prior on d:     ", describe_prior(x$data$prior.d) ,
      "\n   Prior on tau:   ", describe_prior(x$data$prior.tau),
      "\n   Bayes factor (d ~ prior vs. d = 0)      =", x$BF_10["d"],
      "\n   Bayes factor (tau ~ prior vs. tau = 0)  =", x$BF_10["tau"],
      "\n\n")
  print(x$estimates)
}


#' @export
print.meta_bma <- function(x,...){
  cat("### Bayesian Model Averaging of Meta-Analysis ###\n",
      "\n   Fixed H0:  d = 0",
      "\n   Fixed H1:  d ~",describe_prior(x$meta$fixed.H1$data$prior.d),
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
  cat("\n# Bayes factors:\n")
  print(as.data.frame(x$BF, row.names("")))

  cat("\n# Posterior of d:\n")
  print(x$estimates)
}
