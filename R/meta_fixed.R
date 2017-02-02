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
                       ...){

  data_list <- data_list("fixed", y = y, SE = SE, labels = labels,
                         d = d, d.par = d.par)

  jags_samples <- get_samples(data_list, sample = sample, ...)

  # bounds <- bounds_prior(data_list$prior.d, label = "d.fixed")
  integral <- integrate_wrapper(samples = jags_samples$samples,
                                data = data_list)

  res <- list("samples" = jags_samples$samples,
              "integral" = integral,
              "data" = data_list,
              "prior.d" = data_list$prior.d,
              "posterior.d" = NA,
              "logmarginal"  = integral$logml,
              "logmarginal.H0" = loglik_fixed_H0(data_list),
              "BF" = c("d_10" = exp(integral$logml - loglik_fixed_H0(data_list))),
              "estimates" = NA,
              "jagsmodel" = jags_samples$jagsmodel
  )
  class(res) <- "meta_fixed"

  res$posterior.d <- posterior(res, "d")
  res$estimates <- rbind(d = stats_density(res$posterior.d))
  res$data <- res$data[c("y", "SE", "labels")]
  return(res)
}
