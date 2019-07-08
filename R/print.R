print.est <- function(estimates = NULL, what = "", digits = 3){
  if (is.null(estimates)){
    cat("\n  (no summary statistics for parameters computed)\n")
  } else {
    cat("\n# Posterior summary statistics", what,":\n", sep = "")
    print(round(estimates, digits = digits))
  }
}

# ' Print Results of Bayesian Meta-Analysis
# '
# ' @param x a fitted meta-analysis object
# ' @param ... ignored
#' @export
print.meta_fixed <- function (x, digits = 3, ...){
  cat("### Bayesian Fixed-Effects Meta-Analysis ###",
      "\n   Prior on d:   ", describe_prior(x$prior_d, digits) ,
      # "\n   Bayes factor (d ~ prior vs. d = 0) =", round(x$BF, digits = digits),
      "\n")
  cat("\n# Bayes factors:\n")
  print(x$BF, digits = digits)
  print.est(x$estimates, what = " of fixed-effects model", digits = digits)

  if (any(grepl("alpha", rownames(x$estimates))))
    cat("  (Note: moderators in meta-regression are ",
        ifelse(x$jzs$centering, "", "NOT ") , "mean-centered.)\n", sep = "")
}


#' @export
print.meta_random <- function (x, digits = 3, ...){
  cat("### Bayesian Random-Effects Meta-Analysis ###",
      "\n   Prior on d:     ", describe_prior(x$prior_d, digits) ,
      "\n   Prior on tau:   ", describe_prior(x$prior_tau, digits),
      # "\n   Bayes factor (d ~ prior vs. d = 0)      =", x$BF["d_10"],
      # "\n   Bayes factor (tau ~ prior vs. tau = 0)  =", x$BF["tau_10"],
      "\n")

  cat("\n# Bayes factors:\n")
  print(x$BF, digits = digits)
  print.est(x$estimates, what = " of random-effects model", digits = digits)

  if (any(grepl("alpha", rownames(x$estimates))))
    cat("  (Note: moderators in meta-regression are ",
        ifelse(x$jzs$centering, "", "NOT ") , "mean-centered.)\n", sep = "")
}


#' @export
print.meta_bma <- function (x, digits = 3, ...){

  if (length(x$meta) == 2 && all(names(x$meta) %in% c("fixed", "random")) ){
    cat("### Meta-Analysis with Bayesian Model Averaging ###")
    cat("\n   Fixed H0:  d = 0",
        "\n   Fixed H1:  d ~",describe_prior(x$meta$fixed$prior_d, digits),
        "\n   Random H0: d   = 0,  ",
        "\n              tau ~ ", describe_prior(x$meta$random$prior_tau, digits),
        "\n   Random H1: d   ~",describe_prior(x$meta$random$prior_d, digits),
        "\n              tau ~",describe_prior(x$meta$random$prior_tau, digits),"\n")
  } else if (length(x$meta) == 3 &&
             all(names(x$meta) %in% c("fixed", "random", "ordered")) ){
    cat("### Bayesian Meta-Analysis with Order Constraints ###")
    cat("\n   null:    d = 0",
        "\n   fixed:   d ~ ",describe_prior(x$meta$fixed$prior_d, digits),
        "\n   ordered: d      ~ ", describe_prior(x$meta$random$prior_d, digits),
        "\n            tau    ~ ", describe_prior(x$meta$random$prior_tau, digits),
        "\n            dstudy ~ Normal(d,tau) truncated to [",
        paste(bounds_prior(x$meta$random$prior_d), collapse = ","),"]",
        "\n   random:  d      ~ ",describe_prior(x$meta$random$prior_d, digits),
        "\n            tau    ~ ",describe_prior(x$meta$random$prior_tau, digits),
        "\n            dstudy ~ Normal(d,tau)\n", sep="")
  } else {
    cat("### Meta-Analysis with Bayesian Model Averaging ###")
    cat("\n  Overall effect d:\n")
    print(x$prior_d, digits)
  }
  cat("\n# Bayes factors:\n")
  print(x$BF, digits = digits)

  if (length(x$meta) == 2 && all(names(x$meta) %in% c("fixed", "random")) &&
      !is.null(x$inclusion)){
    cat("\n# Bayesian Model Averaging\n")
    cat("  Comparison: (fixed_H1 & random_H1) vs. (fixed_H0 & random_H0)\n")
    cat("  Inclusion Bayes factor:", round(x$inclusion$incl.BF, digits), "\n")
    cat("  Inclusion posterior probability:",
        round(x$inclusion$incl.posterior, digits), "\n")
  }

  cat("\n# Model posterior probabilities:\n")
  tab <- data.frame("prior" = x$prior_models,
                    "posterior" = x$posterior_models,
                    "logml" = x$logml,
                    row.names = names(x$logml))
  print(tab, digits = digits)

  print.est(x$estimates, what = " of average effect size", digits = digits)
}
