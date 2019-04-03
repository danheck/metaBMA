print.est <- function(estimates = NULL, digits = 3){
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
      "\n   Prior on d:   ", describe_prior(x$prior_d) ,
      "\n   Bayes factor (d ~ prior vs. d = 0) =", x$BF,
      "\n\n")
  print.est(x$estimates)
}


#' @export
print.meta_random <- function (x,...){
  cat("### Bayesian Random-Effects Meta-Analysis ###\n",
      "\n   Prior on d:     ", describe_prior(x$prior_d) ,
      "\n   Prior on tau:   ", describe_prior(x$prior_tau),
      "\n   Bayes factor (d ~ prior vs. d = 0)      =", x$BF["d_10"],
      "\n   Bayes factor (tau ~ prior vs. tau = 0)  =", x$BF["tau_10"],
      "\n\n")
  print.est(x$estimates)
}


#' @export
print.meta_bma <- function (x,...){

  cat("### Bayesian Model Averaging of Meta-Analysis ###\n")
  if (!is.null(x$meta$fixed) &&
      !is.null(x$meta$random) ){
    cat("\n   Fixed H0:  d = 0",
        "\n   Fixed H1:  d ~",describe_prior(x$meta$fixed$prior_d),
        "\n   Random H0: d = 0,  ",
        "\n              tau ~ ", describe_prior(x$meta$random$prior_tau),
        "\n   Random H1: d ~",describe_prior(x$meta$random$prior_d),
        "\n              tau ~",describe_prior(x$meta$random$prior_tau),"\n")
  } else {
    cat("  Mean effect d:\n")
    print(x$prior_d)
  }
  cat("\n# Bayes factors:\n")
  print(as.data.frame(x$BF, row.names("")))

  cat("\n# Model posterior probabilities:\n")
  tab <- data.frame("prior" = x$prior_models,
                    "posterior" = x$posterior_models,
                    "logml" = x$logml,
                    row.names = names(x$logml))
  print(tab)

  print.est(x$estimates)
}
