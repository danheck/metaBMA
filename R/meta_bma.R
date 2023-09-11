#' Model Averaging for Meta-Analysis
#'
#' Fits random- and fixed-effects meta-analyses and performs Bayesian model
#' averaging for H1 (d != 0) vs. H0 (d = 0).
#'
#' Bayesian model averaging for four meta-analysis models: Fixed- vs.
#' random-effects and H0 (\eqn{d=0}) vs. H1 (e.g., \eqn{d>0}).
#' For a primer on Bayesian model-averaged meta-analysis,
#' see Gronau, Heck, Berkhout, Haaf, and Wagenmakers (2020).
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
#' @param d \code{prior} distribution on the average effect size \code{d}. The
#'   prior probability density function is defined via \code{\link{prior}}.
#' @param tau \code{prior} distribution on the between-study heterogeneity
#'   \code{tau} (i.e., the standard deviation of the study effect sizes
#'   \code{dstudy} in a random-effects meta-analysis. A (nonnegative) prior
#'   probability density function is defined via \code{\link{prior}}.
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
#'   many MCMC samples are required (e.g., \code{logml_iter=10000, warmup=1000}).
#' @param summarize how to estimate parameter summaries (mean, median, SD,
#'   etc.): Either by numerical integration (\code{summarize = "integrate"}) or
#'   based on MCMC/Stan samples (\code{summarize = "stan"}).
#' @param ci probability for the credibility/highest-density intervals.
#' @param rel.tol relative tolerance used for numerical integration using
#'   \code{\link[stats]{integrate}}. Use \code{rel.tol=.Machine$double.eps} for
#'   maximal precision (however, this might be slow).
#' @param logml_iter number of iterations (per chain) from the posterior
#'   distribution of \code{d} and \code{tau}. The samples are used for computing
#'   the marginal likelihood of the random-effects model with bridge sampling
#'   (if \code{logml="stan"}) and for obtaining parameter estimates (if
#'   \code{summarize="stan"}). Note that the argument \code{iter=2000} controls
#'   the number of iterations for estimation of the random-effect parameters per
#'   study in random-effects meta-analysis.
#' @param silent_stan whether to suppress the Stan progress bar.
#' @param ... further arguments passed to \code{rstan::sampling} (see
#'   \code{\link[rstan]{stanmodel-method-sampling}}). Relevant MCMC settings
#'   concern the number of warmup samples that are discarded
#'   (\code{warmup=500}), the total number of iterations per chain
#'   (\code{iter=2000}), the number of MCMC chains (\code{chains=4}), whether
#'   multiple cores should be used (\code{cores=4}), and control arguments that
#'   make the sampling in Stan more robust, for instance:
#'   \code{control=list(adapt_delta=.97)}.
#'
#' @details
#' By default, the log-marginal likelihood is computed by numerical integration
#' (\code{logml="integrate"}). This is relatively fast and gives precise,
#' reproducible results. However, for extreme priors or data (e.g., very small
#' standard errors), numerical integration is not robust and might provide
#' incorrect results. As an alternative, the log-marginal likelihood can be
#' estimated using MCMC/Stan samples and bridge sampling (\code{logml="stan"}).
#'
#' To obtain posterior summary statistics for the average effect size \code{d}
#' and the heterogeneity parameter \code{tau}, one can also choose between
#' numerical integration (\code{summarize="integrate"}) or MCMC sampling in Stan
#' (\code{summarize="stan"}). If any moderators are included in a model, both
#' the marginal likelihood and posterior summary statistics can only be computed
#' using Stan.
#'
#' @seealso \link{meta_fixed}, \link{meta_random}
#' @template ref_gronau2017
#' @template ref_gronau2021
#' @template ref_berkhout2023
#'
#' @examples
#' \donttest{
#' ### Bayesian Model-Averaged Meta-Analysis (H1: d>0)
#' data(towels)
#' set.seed(123)
#' mb <- meta_bma(logOR, SE, study, towels,
#'   d = prior("norm", c(mean = 0, sd = .3), lower = 0),
#'   tau = prior("invgamma", c(shape = 1, scale = 0.15))
#' )
#' mb
#' plot_posterior(mb, "d")
#' }
#' @export
meta_bma <- function(
    y, SE, labels, data,
    d = prior("cauchy", c(location = 0, scale = 0.707)),
    tau = prior("invgamma", c(shape = 1, scale = 0.15)),
    rscale_contin = 0.5,
    rscale_discrete = 0.707,
    centering = TRUE,
    prior = c(1, 1, 1, 1),
    logml = "integrate",
    summarize = "stan",
    ci = .95,
    rel.tol = .Machine$double.eps^.3,
    logml_iter = 5000,
    silent_stan = TRUE,
    ...
) {

  check_deprecated(list(...)) # error: backwards compatibility

  dl <- data_list(
    model = "random", y = y, SE = SE, labels = labels, data = data,
    args = as.list(match.call())[-1]
  )

  # cat(format(Sys.time()), "--- Fit fixed-effects meta-analysis\n")
  fixed_H1 <- meta_fixed(y, SE, labels,
    data = dl, d = d, logml = logml,
    summarize = summarize, ci = ci, rel.tol = rel.tol,
    silent_stan = silent_stan, ...
  )

  # cat(format(Sys.time()), "--- Fit random-effects meta-analysis\n")
  random_H1 <- meta_random(y, SE, labels,
    data = dl, d = d, tau = tau, logml = logml,
    summarize = summarize, ci = ci, rel.tol = rel.tol,
    silent_stan = silent_stan, logml_iter = logml_iter, ...
  )

  # random_H0 <- meta_random(y, SE, labels, data = dl, d = prior("0"), tau = tau, logml = logml,
  #                          summarize = summarize, ci = ci, rel.tol = rel.tol,
  #                          silent_stan = silent_stan, logml_iter = logml_iter, ...)

  # model averaging for:  d | H1
  meta <- list("fixed" = fixed_H1, "random" = random_H1)
  meta_bma <- bma(meta,
    prior = prior, parameter = "d",
    summarize = summarize, ci = ci, rel.tol = rel.tol
  )

  # bma(list(), prior = prior, parameter = "d",
  #     summarize = "stan", ci = ci, rel.tol = rel.tol)

  # inclusion bayes factors etc.
  meta_bma$inclusion <- inclusion(meta_bma$logml, include = c(2, 4), prior = prior)
  meta_bma$prior_models <- prior / sum(prior)
  meta_bma$posterior_models <- meta_bma$inclusion$posterior
  meta_bma$BF <- make_BF(meta_bma$logml)
  meta_bma
}
