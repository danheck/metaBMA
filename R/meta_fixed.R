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
#'                  d = "halfnorm", d.par = c(0, .3), sample = 0,
#'                  summarize = "integrate")
#' mf
#' plot_posterior(mf)
#' plot_forest(mf)
#' @export
meta_fixed <- function(y, SE, labels, data,
                       d = "halfnorm", d.par = c(mean=0, sd=0.3),
                       # d = prior("halfnorm", c(mean=0, sd=0.3)
                       sample = 5000, summarize = "integrate",
                       rel.tol = .Machine$double.eps^.5, ...){
  summarize <- match.arg(summarize, c("jags", "integrate", "none"))
  if (summarize == "jags" && sample <= 0)
    stop("if summarize = 'jags', it is necessary to use sample > 0.")

  data_list <- data_list("fixed", y = y, SE = SE, labels = labels, data = data,
                         d = d, d.par = d.par, args = as.list(match.call()))

  logml <- integrate_wrapper(data = data_list, rel.tol = rel.tol)

  meta <- list("data" = data_list,
               "prior.d" = data_list$prior.d,
               "posterior.d" = NA,
               "logmarginal"  = logml,
               "logmarginal.H0" = loglik_fixed_H0(data_list),
               "BF" = c("d_10" = exp(logml - loglik_fixed_H0(data_list))),
               "estimates" = NULL)
  class(meta) <- "meta_fixed"

  if (sample > 0){
    jags_samples <- get_samples(data = data_list, sample = sample, ...)
    if (summarize == "jags")
      meta$estimates <- rbind("d" = stats_samples(jags_samples$samples, "d.fixed"))
    meta$samples <- jags_samples$samples
    meta$jagsmodel <- jags_samples$jagsfile
  }

  meta$posterior.d <- posterior(meta, "d", rel.tol = rel.tol)
  meta$posterior.d <- check_posterior(meta$posterior.d, meta, "d.fixed")
  if (summarize == "integrate" || is.null(meta$estimates))
    meta$estimates <- rbind("d" = stats_density(meta$posterior.d, rel.tol = rel.tol))
  if (anyNA(meta$estimates) && sample > 0){
    warning("Summary statistics computed with 'integrate' contain missings.\n",
            "  Summary statistics of the JAGS samples are reported instead.")
    meta$estimates <- rbind("d" = stats_samples(jags_samples$samples, "d.fixed"))
  }

  meta$data <- meta$data[c("y", "SE", "labels")]
  meta
}
