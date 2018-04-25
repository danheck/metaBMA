#' Bayesian Random-Effects Meta-Analysis
#'
#' Bayesian meta-analysis assuming that the effect size \eqn{d} varies
#' across studies with standard deviation \eqn{\tau} (i.e., a random-effects model).
#'
#' @inheritParams meta_bma
#'
#' @examples
#' data(towels)
#' ### Bayesian Random-Effects Meta-Analysis
#' mr <- meta_random(logOR, SE, study, data = towels,
#'                   d = prior("norm", c(mean=0, sd=.3)),
#'                   tau = prior("t", c(mu=0, sigma=.5, nu=1), lower=0),
#'                   rel.tol = .Machine$double.eps^.2)
#'                   # (no summary: only for CRAN checks)
#' mr
#' plot_posterior(mr)
#' @export
meta_random <- function (y, SE, labels, data,
                         d = prior("norm", c(mean=0, sd=.3), lower=0),
                         tau  = prior("t", c(mu=0, sigma=.5, nu=1), lower=0),
                         rscale_contin = 1/2, rscale_discrete = sqrt(2)/2,
                         centering = TRUE,
                         logml = "integrate", summarize = "stan", ci = .95,
                         rel.tol = .Machine$double.eps^.3, ...){

  logml <- match.arg(logml, c("integrate", "stan"))
  data_list <- data_list("random", y = y, SE = SE, labels = labels, data = data,
                         args = as.list(match.call()))

  d <- check_prior(d)
  tau <- check_prior(tau, 0)
  meta <- list("model" = data_list$model,
               "data" = data_list, "prior_d" = d, "prior_tau" = tau,
               "jzs" = list(rscale_contin = rscale_contin,
                            rscale_discrete = rscale_discrete,
                            centering = centering),
               "posterior_d" = NULL, "posterior_tau" = NULL,
               "logml"  = NA, "BF" = NULL, "estimates" = NULL)
  class(meta) <- "meta_random"

  if (attr(d, "family") %in% priors_stan())
    meta$stan_messages <- capture.output(
      meta$stanfit <- meta_stan(data_list, d = d, tau = tau, jzs = meta$jzs, ...))

  if (logml == "integrate" || !attr(d, "family") %in% priors_stan())
    meta$logml <- integrate_wrapper(data_list, d, tau, rel.tol = rel.tol)
  meta <- meta_bridge_sampling(meta, logml, ...)

  meta$posterior_d <- posterior(meta, "d", rel.tol = rel.tol)
  meta$posterior_tau <- posterior(meta, "tau", rel.tol = rel.tol)

  meta$estimates <- summary_meta(meta, summarize)
  meta$BF <- c(d_10   = meta$prior_d(0)   / meta$posterior_d(0),
               tau_10 = meta$prior_tau(0) / meta$posterior_tau(0))
  meta
}

