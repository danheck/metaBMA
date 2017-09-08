print.est <- function(estimates = NULL, digits = 4){
  if (is.null(estimates)){
    cat("\n  (no summary statistics for parameters computed)\n")
  }else{
    cat("\n# Posterior summary statistics:\n")
    print(round(estimates, digits = digits))
  }
}

# ' Print Results of Bayesian Meta-Analysis
# '
# ' @param x a fitted meta-analysis object
# ' @param ... ignored
#' @export
print.meta_fixed <- function (x,...){
  cat("### Bayesian Fixed-Effects Meta-Analysis ###\n",
      "\n   Prior on d:   ", describe_prior(x$prior.d) ,
      "\n   Bayes factor (d ~ prior vs. d = 0) =", x$BF,
      "\n\n")
  print.est(x$estimates)
}


#' @export
print.meta_random <- function (x,...){
  cat("### Bayesian Random-Effects Meta-Analysis ###\n",
      "\n   Prior on d:     ", describe_prior(x$prior.d) ,
      "\n   Prior on tau:   ", describe_prior(x$prior.tau),
      "\n   Bayes factor (d ~ prior vs. d = 0)      =", x$BF["d_10"],
      "\n   Bayes factor (tau ~ prior vs. tau = 0)  =", x$BF["tau_10"],
      "\n\n")
  print.est(x$estimates)
}


#' @export
print.meta_bma <- function (x,...){

  cat("### Bayesian Model Averaging of Meta-Analysis ###\n")
  if (!is.null(x$meta$fixed.H1) &&
      !is.null(x$meta$random.H0) &&
      !is.null(x$meta$random.H1) ){
    cat("\n   Fixed H0:  d = 0",
        "\n   Fixed H1:  d ~",describe_prior(x$meta$fixed.H1$prior.d),
        "\n   Random H0: d = 0,  ",
        "\n              tau ~ ", describe_prior(x$meta$random.H0$prior.tau),
        "\n   Random H1: d ~",describe_prior(x$meta$random.H1$prior.d),
        "\n              tau ~",describe_prior(x$meta$random.H1$prior.tau),"\n")
  } else {
    cat("  Mean effect d:\n")
    tmp <- sapply(x$prior.d, print)
  }
  cat("\n# Bayes factors:\n")
  print(as.data.frame(x$BF, row.names("")))

  cat("\n# Model posterior probabilities:\n")
  tab <- data.frame(prior = x$prior.models,
                    posterior = x$posterior.models,
                    marginal = exp(x$logmarginal),
                    row.names = names(x$logmarginal))
  print(tab)

  print.est(x$estimates)
}
