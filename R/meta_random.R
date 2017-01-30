#' Bayesian Random-Effects Meta-Analysis
#'
#' @inheritParams meta_bma
#'
#' @examples
#' data(towels)
#' ### Bayesian Random-Effects Meta-Analysis (H0: No effect, d=0)
#' mr <- meta_random(towels$logOR, towels$variance,
#'                   d = "0",  ### random-efects H0 ###
#'                   tau = "halfcauchy",tau.par = .5,
#'                   sample=300, n.chains = 2,
#'                   imethod = "integrate")
#' mr
#' @export
meta_random <- function(y,
                        V,
                        d = "norm",
                        d.par = c(0, .3),
                        tau = "halfcauchy",
                        tau.par = .5,
                        marginal = "integrate",
                        ...){

  data_list <- list(y = y, V = V,
                    model = "random",
                    prior.d = list(name = d, par = d.par),
                    prior.tau = list(name = tau, par = tau.par))

  jags_samples <- get_samples(data_list, ...)
  samples <- jags_samples$samples

  bounds <- list(bounds_prior(data_list$prior.d, label = "d.random"),
                 bounds_prior(data_list$prior.tau, label = "tau"))
  bounds <- bounds[ !sapply(bounds, is.null)]

  integral <- integrate_wrapper(samples = samples,
                                log_posterior = log_posterior,
                                data = data_list,
                                lb = sapply(bounds, function(b) b[1]),
                                ub = sapply(bounds, function(b) b[2]),
                                method = marginal)

  ests <- rbind(d.random = summary_ests(samples, "d.random"),
                tau = summary_ests(samples, "tau"))


  res <- list(samples = samples,
              integral = integral,
              data = data_list,
              logmarginal  = integral$logml,
              estimates = ests)
  class(res) <- "meta_random"
  return(res)
}

