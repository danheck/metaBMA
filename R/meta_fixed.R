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
#' mf <- meta_fixed(towels$logOR, towels$variance,
#'                  d = "halfnorm", d.par = c(0, .3),
#'                  sample=300, n.chains = 2)
#' mf
#' @export
meta_fixed <- function(y,
                       V,
                       labels,
                       d = "halfnorm",
                       d.par = c(mean=0, sd=0.3),
                       marginal = "integrate",
                       estimate = "integrate",
                       ...){
  if(missing(labels))
    labels <- paste0("study",1:length(y))
  data_list <- list(y = y, V = V, labels = labels,
                    model = "fixed",
                    prior.d = list(name = d, par = d.par))

  if(estimate == "jags" || marginal == "bridge")
    jags_samples <- get_samples(data_list, ...)
  else
    jags_samples <- NULL

  bounds <- bounds_prior(data_list$prior.d, label = "d.fixed")
  integral <- integrate_wrapper(samples = jags_samples$samples,
                                data = data_list,
                                lb = bounds[1],
                                ub = bounds[2],
                                method = marginal)

  res <- list(samples = jags_samples$samples,
              integral = integral,
              data = data_list,
              logmarginal  = integral$logml,
              logmarginal.H0 = loglik_fixed_H0(data_list),
              BF = c(d_10 = exp(integral$logml - loglik_fixed_H0(data_list))),
              estimates = NA,
              jagsmodel = jags_samples$jagsmodel
  )
  class(res) <- "meta_fixed"
  if(estimate == "integrate")
    res$estimates <- rbind(d = stats_density(posterior(res, "d"),
                                                   bounds[1], bounds[2]))
  else
    res$estimates <- rbind(d = summary_ests(jags_samples$samples))

  return(res)
}
