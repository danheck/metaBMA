#' Bayesian Fixed-Effects Meta-Analysis
#'
#' Runs a Bayesian meta-analysis assuming that the mean effect \eqn{d} in each study is identical (i.e., a fixed-effects analysis).
#'
#' @inheritParams meta_bma
#' @inheritParams meta_random
#'
#' @examples
#' data(towels)
#' ### Bayesian Fixed-Effects Meta-Analysis (H1: d>0 Cauchy)
#' mf <- meta_fixed(towels$logOR, towels$SE, towels$study,
#'                  d = "halfnorm", d.par = c(0, .3))
#' mf
#' plot_posterior(mf)
#' plot_forest(mf)
#' @export
meta_fixed <- function(y,
                       SE,
                       labels = NULL,
                       d = "halfnorm",
                       d.par = c(mean=0, sd=0.3),
                       sample = 0,
                       summarize = "integrate",
                       ...){
  if (summarize == "jags" && sample <= 0)
    stop("if summarize = 'jags', it is necessary to use sample > 0.")

  data_list <- data_list("fixed", y = y, SE = SE, labels = labels,
                         d = d, d.par = d.par)

  integral <- integrate_wrapper(data = data_list)

  meta <- list("data" = data_list,
               "prior.d" = data_list$prior.d,
               "posterior.d" = NA,
               "logmarginal"  = integral$logml,
               "logmarginal.H0" = loglik_fixed_H0(data_list),
               "BF" = c("d_10" = exp(integral$logml -
                                       loglik_fixed_H0(data_list))),
               "estimates" = NULL,
               "integral" = integral)
  class(meta) <- "meta_fixed"

  meta$posterior.d <- posterior(meta, "d")
  if (summarize == "integrate")
    meta$estimates <- rbind("d" = stats_density(meta$posterior.d))

  if (sample > 0){
    jags_samples <- get_samples(data = data_list, sample = sample, ...)
    if (summarize == "jags")
      meta$estimates <- rbind("d" = stats_samples(jags_samples$samples, "d.fixed"))
    meta$samples <- jags_samples$samples
    meta$jagsmodel <- jags_samples$jagsfile
  }

  meta$posterior.d <- check_posterior(meta$posterior.d, meta, "d.fixed")
  meta$data <- meta$data[c("y", "SE", "labels")]
  return(meta)
}
