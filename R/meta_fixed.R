#' Bayesian Fixed-Effects Meta-Analysis
#'
#' Runs a Bayesian meta-analysis assuming that the mean effect \eqn{d} in each
#' study is identical (i.e., a fixed-effects analysis).
#'
#' @inheritParams meta_bma
#' @inheritParams meta_random
#'
#' @examples
#' data(towels)
#' ### Bayesian Fixed-Effects Meta-Analysis (H1: d>0 Cauchy)
#' mf <- meta_fixed(logOR, SE, study, data = towels,
#'                  d = prior("norm", c(mean=0, sd=.3), lower=0))
#' mf
#' plot_posterior(mf)
#' plot_forest(mf)
#' @export
meta_fixed <- function(y, se, labels, data,
                       d = prior("norm", c(mean = 0, sd = .3), lower = 0),
                       rscale_contin = 1/2, rscale_discrete = sqrt(2)/2,
                       centering = TRUE,
                       logml = "integrate", summarize = "integrate", ci = .95,
                       rel.tol = .Machine$double.eps^.3, ...){

  logml <- match.arg(logml, c("integrate", "stan"))
  data_list <- data_list("fixed", y = y, se = se, labels = labels, data = data,
                         args = as.list(match.call()))

  d <- check_prior(d)
  tau <- prior("0", c(), label = "tau")

  meta <- list("model" = data_list$model,
               "data" =  data_list, "prior_d" = d, "prior_tau" = tau,
               "jzs" = list(rscale_contin = rscale_contin,
                            rscale_discrete = rscale_discrete,
                            centering = centering),
               "posterior_d" = NA, "posterior_tau" = tau,
               "logml"  = NA,  "BF" = NULL, "estimates" = NULL)
  class(meta) <- "meta_fixed"

  if (attr(d, "family") %in% priors_stan())
    meta$stan_messages <- capture.output(
      meta$stanfit <- meta_stan(data_list, d = d, jzs=meta$jzs, ...))

  if (logml == "integrate" || !attr(d, "family") %in% priors_stan())
    meta$logml <- integrate_wrapper(data_list, d, rel.tol = rel.tol)
  meta <- meta_bridge_sampling(meta, logml, ...)

  # not for fixed_jzs
  meta$posterior_d <- posterior(meta, "d", rel.tol = rel.tol)

  meta$estimates <- summary_meta(meta, summarize)

  if (data_list$model == "fixed")  # only without jzs:
    meta$BF <- c("d_10" = exp(meta$logml - loglik_fixed_H0(data_list)))
  else
    meta$BF <- c("d_10" = d(0) / meta$posterior_d(0))
  meta
}
