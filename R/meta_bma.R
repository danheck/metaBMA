#' Model Averaging for Meta-Analysis
#'
#' Fits random- and fixed-effects meta-anayses and performs Bayesian model
#' averaging for H1 (d != 0) vs. H0 (d = 0).
#'
#' Bayesian model averaging for four meta-analysis models: Fixed- vs.
#' random-effects and H0 (\eqn{d=0}) vs. H1 (e.g., \eqn{d>0}).
#'
#' @param y effect size per study. Can be provided as (1) a numeric vector, (2)
#'   the quoted or unquoted name of the variable in \code{data}, or (3) a
#'   \code{\link[stats]{formula}} to include discrete or continuous moderator
#'   variables.
#' @param SE standard error of effect size for each study. Can be a numeric
#'   vector or the quoted or unquoted name of the variable in \code{data}
#' @param labels optional: character values with study labels. Can be a
#'   character vector or the quoted or unquoted name of the variable in
#'   \code{data}
#' @param data data frame containing the variables for effect size \code{y},
#'   standard error \code{SE}, \code{labels}, and moderators per study.
#'
#' @param d \code{prior} distribution on the average effect size \eqn{d}. The
#'   prior probability density function is defined via \code{\link{prior}}.
#' @param tau \code{prior} distribution on the standard deviation \eqn{\tau} of
#'   the study effect sizes in a random-effects meta-analysis (i.e., the SD of
#'   \eqn{d} across studies). A (nonnegative) prior probability density function
#'   is defined via \code{\link{prior}}.
#' @param rscale_discrete scale parameter of the JZS prior for discrete
#'   moderators.
#' @param rscale_contin scale parameter of the JZS prior for the continuous
#'   covariates.
#' @param centering whether continuous moderators are centered.
#' @param prior prior probabilities over models (possibly unnormalized) in the
#'   order \code{c(fixed_H0, fixed_H1, random_H0, random_H1)}. For instance, if
#'   we expect fixed effects to be two times as likely as random effects and H0
#'   and H1 to be equally likely: \code{prior = c(2,2,1,1)}.
#'
#' @param logml how to estimate the log-marginal likelihood: either by numerical
#'   integration (\code{"integrate"}) or by bridge sampling using MCMC/Stan
#'   samples (\code{"stan"}). To obtain high precision with \code{logml="stan"},
#'   many MCMC samples are required (e.g., \code{iter=10000, warmup=1000}).
#' @param summarize how to estimate parameter summaries (mean, median, SD,
#'   etc.): Either by numerical integration (\code{summarize = "integrate"}) or
#'   based on MCMC/Stan samples (\code{summarize = "stan"}).
#' @param ci probability for the credibility/highest-density intervals.
#' @param rel.tol relative tolerance used for numerical integration using
#'   \code{\link[stats]{integrate}}. Use \code{rel.tol=.Machine$double.eps} for
#'   maximal precision (however, this might be slow).
#' @param logml_iter number of iterations from the posterior of d and tau
#'   for computing the marginal likelihood for the random-effects model with bridge sampling.
#'   Note that the argument \code{iter=2000} controls the number of iterations
#'   for parameter estimation of the random effects.
#' @param ... further arguments passed to \code{rstan::sampling} (see
#'   \code{\link[rstan]{stanmodel-method-sampling}}). For instance:
#'   \code{warmup=500}, \code{chains=4}, \code{control=list(adapt_delta=.95)}).
#'
#' @details By default, the log-marginal likelihood is computed by numerical
#' integration. This is relatively fast and gives precise, reproducible results.
#' However, for extreme priors or data, this might be robust. Hence, as an
#' alternative, the log-marginal likelihood can be estimated using MCMC/Stan
#' samples and bridge sampling.
#'
#' Note that the same two options are available to obtain summary statistics of
#' the posterior distributions of the average effect size \eqn{d} and the
#' heterogeneity parameter \eqn{tau}.
#'
#' @examples
#' # Note: The following code optimizes speed (for CRAN checks).
#' #       The settings are not suitable for actual data analysis!
#'
#' data(towels)
#' set.seed(123)
#' mb <- meta_bma(logOR, SE, study, towels,
#'                d = prior("norm", c(mean=0, sd=.3), lower=0),
#'                tau = prior("t", c(location=0, scale=.5, nu=1), lower=0),
#'                rel.tol = .Machine$double.eps^.15)  # speed!
#' mb
#' plot_posterior(mb, "d")
#' @seealso \link{meta_fixed}, \link{meta_random}
#' @template ref_gronau2017
#' @export
meta_bma <- function (y, SE, labels, data,
                      d = prior("norm", c(mean = 0, sd = .3), lower = 0),
                      tau  = prior("t", c(location = 0, scale = .5, nu = 1), lower = 0),
                      rscale_contin = 1/2, rscale_discrete = sqrt(2)/2,
                      centering = TRUE, prior = c(1,1,1,1),
                      logml = "integrate", summarize = "stan", ci = .95,
                      rel.tol = .Machine$double.eps^.3,
                      logml_iter = 5000, ...){

  dl <- data_list(model = "random", y = y, SE = SE, labels = labels, data = data,
                  args = as.list(match.call()))

  # fit meta-analysis models
  cat(format(Sys.time()), "--- Fit fixed-effects meta-analysis\n")
  fixed_H1 <- meta_fixed(y, SE, labels, data = dl, d = d, logml = logml,
                         summarize = summarize, ci = ci, rel.tol = rel.tol, ...)
  cat(format(Sys.time()), "--- Fit random-effects meta-analysis\n")
  random_H1 <- meta_random(y, SE, labels, data = dl, d = d, tau = tau, logml = logml,
                           summarize = summarize, ci = ci, rel.tol = rel.tol,
                           logml_iter = logml_iter, ...)

  # model averaging for:  d | H1
  meta <- list("fixed" = fixed_H1, "random" = random_H1)
  meta_bma <- bma(meta, prior = prior[c(2, 4)], parameter = "d",
                  summarize = summarize, ci = ci, rel.tol = rel.tol)

  # inclusion bayes factors etc.
  logml_fixed_H0 <- fixed_H1$logml - log(fixed_H1$BF["d_10"])
  logml_random_H0 <- random_H1$logml - log(random_H1$BF["d_10"])
  meta_bma$logml <- c("fixed_H0" = matrix(logml_fixed_H0), # CHECK loglik_fixed_H0(fixed_H1$data),
                      "fixed_H1" = fixed_H1$logml,
                      "random_H0" = matrix(logml_random_H0),
                      "random_H1" = random_H1$logml)
  meta_bma$inclusion <- inclusion(meta_bma$logml, include = c(2,4), prior = prior)
  meta_bma$prior_models <- prior/sum(prior)
  meta_bma$posterior_models <- meta_bma$inclusion$posterior

  meta_bma$BF <- list("d_10_fixed" = c(fixed_H1$BF["d_10"]),
                      "d_10_random" = c(random_H1$BF["d_10"]),
                      "d_10_averaged" = meta_bma$inclusion$incl.BF)
                      # "tau_10_H1" = c(random_H1$BF["tau_10"])



  meta_bma
}
