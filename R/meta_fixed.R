#' Bayesian Fixed-Effects Meta-Analysis
#'
#' @inheritParams meta_bma
#' @inheritParams meta_random
#'
#' @examples
#' data(towels)
#' ### Bayesian Fixed-Effects Meta-Analysis (H1: d>0 Cauchy)
#' mf <- meta_fixed(towels$logOR, towels$variance,
#'                  d = "halfnorm", d.par = c(0, .3),
#'                  sample=300, n.chains = 2,
#'                  marginal = "integrate")
#' mf
#' @export
meta_fixed <- function(y,
                       V,
                       d = "halfnorm",
                       d.par = c(mean=0, sd=0.3),
                       marginal = "integrate",
                       ...){

  data_list <- list(y = y, V = V,
                    model = "fixed",
                    prior.d = list(name = d, par = d.par))

  jags_samples <- get_samples(data_list,  ...)
  samples <- jags_samples$samples

  bounds <- bounds_prior(data_list$prior.d, label = "d.fixed")

  integral <- integrate_wrapper(samples = samples,
                                log_posterior = log_posterior,
                                data = data_list,
                                lb = bounds[1],
                                ub = bounds[2],
                                method = marginal)

  res <- list(samples = samples,
              integral = integral,
              data = data_list,
              logmarginal  = integral$logml,
              logmarginal.H0 = loglik_fixed_H0(data_list),
              estimates = rbind(d.fixed = summary_ests(samples)),
              jagsmodel = jags_samples$jagsmodel
  )
  class(res) <- "meta_fixed"
  return(res)
}
