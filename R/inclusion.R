#' Inclusion Bayes Factor
#'
#' Computes the inclusion Bayes factor for two sets of models (e.g., A=\{M1,M2\} vs. B=\{M3,M4\}).
#'
#' @param logml a vector with log-marginal likelihoods. Alternatively, a list with meta-analysis models (fitted via \code{\link{meta_random}} or \code{\link{meta_fixed}})
#' @param include integer vector which models to include in inclusion Bayes factor/posterior probability. If only two marginal likelihoods/meta-analyses are supplied, the Bayes factor BF_\{M1,M2\} is computed by default.
#' @param prior prior probabilities over models (possibly unnormalized). For instance, if the first model is as likely as models 2, 3 and 4 together: \code{prior = c(3,1,1,1)}. The default is a discrete uniform distribution over models.
#'
#' @examples
#' #### Example with simple Normal-distribution models
#' # data:
#' x <- rnorm(50)
#'
#' # Model 1: x ~ Normal(0,1)
#' logm1 <- sum(dnorm(x, log = TRUE))
#' # Model 2: x ~ Normal(1,1)
#' logm2 <-sum(dnorm(x, mean = 1, log = TRUE))
#' # Model 3: x ~ Student-t(df=2)
#' logm3 <-sum(dt(x, df=2, log = TRUE))
#'
#' # BF: Correct (M1) vs. misspecified (M2, M3)
#' inclusion(c(logm1, logm2, logm3), include = 1)
#' @export
inclusion <- function (logml,
                       include = 1,
                       prior = 1){

  if (is.list(logml))
    logml <- sapply(logml, "[[", "logml")

  if (missing(prior) || length(prior) == 1)
    prior <- rep(prior, length(logml))
  if (!is.numeric(prior) || any(prior<0))
    stop ("'prior' must contain nonnegative values!")
  if (length(logml) != length(prior))
    stop ("'logml' and 'prior' must be of identical length.")

  prior <- prior/sum(prior)

  denom <- sum(exp(logml)*prior)
  posterior <- exp(logml)*prior / denom

  post.incl <- sum(posterior[include])
  prior_incl <- sum(prior[include])

  # model-averaged Bayes factor in favor of effect
  BF.incl <- post.incl/(1 - post.incl)/(prior_incl/(1 - prior_incl))

  res <- list("prior" = prior,
              "posterior" = posterior,
              "incl.prior" = sum(prior[include]),
              "incl.posterior" = post.incl,
              "incl.BF" = BF.incl,
              "include" = include)

  class(res) <- "inclusion"
  return (res)
}


#' @export
print.inclusion <- function(x, ...){

  nn <- names(x$prior)
  if(!is.null(attr(x$posterior, "names")))
    nn <- attr(x$posterior, "names")

  cat("### Inclusion Bayes factor ###\n",
      " ", paste(nn, collapse = ","), "\n",
      "  Prior probabilities:", x$prior, "\n",
      "  Posterior probabilities:", x$posterior, "\n\n",
      "  Set of included models:",
      paste(x$include,collapse = ","), "vs.",
      paste(setdiff(1:length(x$prior), x$include), collapse = ","), "\n",
      "  Inclusion BF:", x$incl.BF, "\n",
      "  Inclusion posterior probability:", x$incl.posterior,"\n")
}
