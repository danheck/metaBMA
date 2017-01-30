
#' Compute Inclusion Bayes Factor
#'
#' @param logml a vector with log marginal likelihoods. Alternatively, a list with fitted meta-analysis models (see \code{\link{meta_random}} and \code{\link{meta_fixed}})
#' @param include integer vector which models to include in inclusion posterior. If only two marginal likelihoods/meta-analyses are supplied, the Bayes factor BF_{M1,M2} is computed by default.
#' @param prior prior probabilities over models (possibly unnormalized). For instance, if the first model is as likely as models 2, 3 and 4 together: \code{prior = c(3,1,1,1)}
#'
#' @examples
#' x <- rnorm(50)
#' # M1: x ~ Normal(0,1)
#' logm1 <- sum(dnorm(x, log = TRUE))
#' # M2: x ~ Normal(1,1)
#' logm2 <-sum(dnorm(x, 2, log = TRUE))
#' # M3: x ~ Student-t(df=2)
#' logm3 <-sum(dt(x, df=1, log = TRUE))
#'
#' # BF: Correct (M1) vs. misspecified (M2, M3)
#' bma(c(logm1, logm2, logm3), include = 1)
#' @export
bma <- function(logml,
                include = 1,
                prior = 1){

  if(missing(prior) || length(prior) == 1)
    prior <- rep(prior, length(logml))
  if(any(prior<0))
    stop("'prior' must contain positive values!")
  prior <- prior/sum(prior)

  if(is.list(logml))
    logml <- sapply(logml, function(meta) meta$logmarginal)

  denom <- sum(exp(logml)*prior)
  posterior <- exp(logml)*prior / denom

  post.incl <- sum(posterior[include])

  # model-averaged Bayes factor in favor of effect
  BF.incl <- post.incl/(1 - post.incl)

  return(list(posterior = posterior,
              prior = prior,
              posterior.incl = post.incl,
              prior.incl = sum(prior[include]),
              BF.inclusion = BF.incl,
              include = include))
}

