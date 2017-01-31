#' Bayesian Random-Effects Meta-Analysis
#'
#' Runs a Bayesian meta-analysis assuming that the effect \eqn{d} varies across studies with variance \eqn{\tau} (i.e., a random-effects analysis).
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
#'                   marginal = "integrate")
#' mr
#' @export
meta_random <- function(y,
                        V,
                        labels,
                        d = "norm",
                        d.par = c(0, .3),
                        tau = "halfcauchy",
                        tau.par = .5,
                        marginal = "integrate",
                        estimate = "integrate",
                        ...){
  if(missing(labels))
    labels <- paste0("study",1:length(y))
  data_list <- list(y = y, V = V, labels = labels,
                    model = "random",
                    prior.d = list(name = d, par = d.par),
                    prior.tau = list(name = tau, par = tau.par))

  if(estimate == "jags" || marginal == "bridge")
    jags_samples <- get_samples(data_list, ...)
  else
    jags_samples <- NULL

  bbd = bounds_prior(data_list$prior.d, "d.random")
  bbt = bounds_prior(data_list$prior.tau, "tau")
  integral <- integrate_wrapper(samples = jags_samples$samples,
                                data = data_list,
                                lb = c(bbd[1], bbt[1]),
                                ub = c(bbd[2], bbt[2]),
                                method = marginal)

  res <- list(samples = jags_samples$samples,
              integral = integral,
              data = data_list,
              logmarginal  = integral$logml,
              estimates = NA,
              BF = NA)
  class(res) <- "meta_random"

  if(estimate == "integrate"){
    res$estimates <- rbind(d = stats_density(posterior(res, "d"),
                                             bbd[1], bbd[2]),
                           tau = stats_density(posterior(res, "tau"),
                                               bbt[1], bbt[2]) )
  }else{
    res$estimates <- rbind(d = summary_ests(jags_samples$samples, "d.random"),
                           tau = summary_ests(jags_samples$samples, "tau"))
  }
  res$BF <- c(d_10 = prior(data_list$prior.d)(0) / posterior(res, "d")(0),
              tau_10 = prior(data_list$prior.tau)(0) / posterior(res, "tau")(0))

  return(res)
}

