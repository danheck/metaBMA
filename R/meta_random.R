#' Bayesian Random-Effects Meta-Analysis
#'
#' Runs a Bayesian meta-analysis assuming that the effect \eqn{d} varies across studies with standard deviation \eqn{\tau} (i.e., a random-effects analysis).
#'
#' @inheritParams meta_bma
#'
#' @examples
#' \dontrun{
#' data(towels)
#' ### Bayesian Random-Effects Meta-Analysis
#' mr <- meta_random(towels$logOR, towels$SE, towels$study,
#'                   d = "norm", d.par = c(0,.3),
#'                   tau = "halfcauchy", tau.par = .5)
#' mr
#' plot_posterior(mr)
#' plot_forest(mr)
#' }
#' @export
meta_random <- function (y,
                         SE,
                         labels,
                         d = "norm",
                         d.par = c(0, .3),
                         tau = "halfcauchy",
                         tau.par = .5,
                         sample = 10000,
                         ...){

  data_list <- data_list("random", y = y, SE = SE, labels = labels,
                         d = d, d.par = d.par, tau = tau, tau.par = tau.par)

  jags_samples <- get_samples(data_list, sample = sample, ...)


  bbd = bounds_prior(data_list$prior.d, "d.random")
  bbt = bounds_prior(data_list$prior.tau, "tau")
  integral <- integrate_wrapper(samples = jags_samples$samples,
                                data = data_list)

  res <- list("samples" = jags_samples$samples,
              "integral" = integral,
              "data" =  data_list,  # still includes priors for posterior()
              "prior.d" = data_list$prior.d,
              "prior.tau" = data_list$prior.tau,
              "posterior.d" = NA,
              "posterior.tau" = NA,
              "logmarginal"  = integral$logml,
              "estimates" = NA,
              "BF" = NA)
  class(res) <- "meta_random"

  res$posterior.d <- posterior(res, "d")
  res$posterior.tau <- posterior(res, "tau")
  res$estimates <- rbind(d = stats_density(res$posterior.d),
                         tau = stats_density(res$posterior.tau))

  res$BF <- c(d_10 = res$prior.d(0) / res$posterior.d(0),
              tau_10 = res$prior.tau(0) / res$posterior.tau(0))

  res$data <- res$data[c("y", "SE", "labels")]
  return (res)
}

