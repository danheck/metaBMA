#' Bayesian Random-Effects Meta-Analysis
#'
#' Runs a Bayesian meta-analysis assuming that the effect size \eqn{d} varies across studies with standard deviation \eqn{\tau} (i.e., a random-effects analysis).
#'
#' @inheritParams meta_bma
#'
#' @examples
#' data(towels)
#' ### Bayesian Random-Effects Meta-Analysis
#' mr <- meta_random(towels$logOR, towels$SE, towels$study,
#'                   d = "norm", d.par = c(0,.3),
#'                   tau = "halfcauchy", tau.par = .5,
#'                   sample = 0, summarize = "none")
#'                   # (no summary: only for CRAN checks)
#' mr
#' plot_posterior(mr)
#' @export
meta_random <- function (y,
                         SE,
                         labels,
                         d = "norm",
                         d.par = c(0, .3),
                         tau = "halfcauchy",
                         tau.par = .5,
                         sample = 10000,
                         summarize = "integrate",
                         rel.tol = .Machine$double.eps^.5,
                         ...){
  if (summarize == "jags" && sample <= 0)
    stop("if summarize = 'jags', it is necessary to use sample > 0.")

  data_list <- data_list("random", y = y, SE = SE, labels = labels,
                         d = d, d.par = d.par, tau = tau, tau.par = tau.par)

  logml <- integrate_wrapper(data = data_list, rel.tol = rel.tol)

  meta <- list("data" = data_list,
               "prior.d" = data_list$prior.d,
               "prior.tau" = data_list$prior.tau,
               "posterior.d" = NULL,
               "posterior.d" = NULL,
               "logmarginal"  = logml,
               "BF" = NULL,
               "estimates" = NULL)
  class(meta) <- "meta_random"

  meta$posterior.d <- posterior(meta, "d", rel.tol = rel.tol)
  meta$posterior.tau <- posterior(meta, "tau", rel.tol = rel.tol)

  if (summarize == "integrate")
    meta$estimates <- rbind(d = stats_density(meta$posterior.d, rel.tol = rel.tol),
                            tau = stats_density(meta$posterior.tau, rel.tol = rel.tol))

  if (sample > 0){
    jags_samples <- get_samples(data = data_list, sample = sample, ...)
    if (summarize == "jags")
      meta$estimates <- rbind("d" = stats_samples(jags_samples$samples, "d.random"),
                              "tau" = stats_samples(jags_samples$samples, "tau"))
    meta$samples <- jags_samples$samples
    meta$jagsmodel <- jags_samples$jagsfile
  }

  meta$posterior.d <- check_posterior(meta$posterior.d, meta, "d.random")
  meta$posterior.tau <- check_posterior(meta$posterior.tau, meta, "tau")

  meta$BF <- c(d_10 = meta$prior.d(0) / meta$posterior.d(0),
               tau_10 = meta$prior.tau(0) / meta$posterior.tau(0))

  meta$data <- meta$data[c("y", "SE", "labels")]
  return (meta)
}

