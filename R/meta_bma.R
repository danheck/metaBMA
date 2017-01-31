#' Model Averaging for Meta-Analysis
#'
#' Fits random- and fixed-effects meta-anayses and performs Bayesian model averaging for H1 (d != 0) vs. H0 (d = 0).
#'
#' Bayesian model averaging for four meta-analysis models: Fixed- vs. random-effects and H0 (\eqn{d=0}) vs. H1 (e.g., \eqn{d>0}).
#'
#' @param prior prior probabilities over models (possibly unnormalized) in the order \code{c(fixed.H0, fixed.H1, random.H0, random.H1)}. For instance, if we expect fixed effects to be two times as likely as random effects and H0 and H1 to be equally likely: \code{prior = c(2,2,1,1)}
#' @param y mean in each study
#' @param V variance in each study
#' @param labels optional: character values with study labels
#' @param d type of prior for mean effect \eqn{d} (see \code{\link{prior}})
#' @param d.par prior parameters for \eqn{d}
#' @param tau type of prior for study-variance \eqn{\tau} in random-effects meta-analysis (i.e., the variance of d across studies; see \code{\link{prior}})
#' @param tau.par prior parameters for \eqn{\tau}
#' @param marginal how to integrate marginal likelihood (\code{"bridge"} or \code{"integrate"})
#' @param estimate whether to use JAGS or analytical solutions to estimate parameters
#' @param sample number of samples in JAGS + brdige sampling after burn-in and thinning (see \code{\link[runjags]{run.jags}})
# @param burnin number of burn-in samples in JAGS
# @param n.chains number of chains in JAGS
# @param thin thinning rate in JAGS
# @param targetLength xxx
#' @param resample how many samples should be resampled and stored from JAGS (e.g., for plotting; with replacement).
#' @param progress wheter to print progress of fitting the models
#' @param ... arguments passed to \link[runjags]{run.jags} (e.g., MCMC parameters such as \code{sample}, \code{burnin}, \code{n.chains}, \code{thin} or \code{method="parallel"})
#'
#' @seealso \link{meta_default}, \link{meta_fixed}, \link{meta_random}
#' @export
meta_bma <- function(y,
                     V,
                     labels,
                     d = "norm",
                     d.par = c(0, .3),
                     tau = "halfcauchy",
                     tau.par=.5,
                     prior = c(1,1,1,1),
                     marginal = "integrate",
                     estimate = "integrate",
                     sample = 20000,
                     resample = 5000,
                     progress = TRUE,
                     ...){
  if(missing(labels))
    labels <- paste0("study",1:length(y))
  data_list <- list(y = y, V = V, labels = labels,
                    model="averaged",
                    prior.d = list(name = d, par = d.par),
                    prior.tau = list(name = tau, par = tau.par),
                    prior.models = prior/sum(prior))

  #-------------------------------------------------------------------------------
  # run meta-analyses
  #-------------------------------------------------------------------------------

  logml.fixed.H0 <- loglik_fixed_H0(data_list)

  if(progress) cat("### Fixed effects H1 ###\n")
  m.fixed.H1 <- meta_fixed(y, V, d=d, d.par = d.par, sample = sample,
                           marginal = marginal, estimate = estimate, ...)

  if(progress) cat("### Random effects H0 ###\n")
  m.random.H0 <- meta_random(y, V, d = "0", tau = tau, marginal = marginal, estimate = estimate,
                             tau.par=tau.par, sample = sample, ...)

  if(progress) cat("### Random effects H1 ###\n")
  m.random.H1 <- meta_random(y, V, d = d, d.par = d.par, marginal = marginal, estimate = estimate,
                             tau = tau, tau.par=tau.par, sample = sample, ...)

  #-------------------------------------------------------------------------------
  # store results
  #-------------------------------------------------------------------------------

  metas <- list(fixed.H0 = list(logmarginal = logml.fixed.H0),
                fixed.H1 = m.fixed.H1,
                random.H0 = m.random.H0,
                random.H1 = m.random.H1)

  bma <- bma(metas, include = c(2,4), prior = prior)

  BF <- list(d_10_fixed = c(m.fixed.H1$BF["d_10"]), #exp(m.fixed.H1$logmarginal - logml.fixed.H0),
             d_10_random = c(m.random.H1$BF["d_10"]), #exp(m.random.H1$logmarginal - m.random.H0$logmarginal),
             d_10_averaged = bma$BF.inclusion,
             tau_10_random = c(m.random.H1$BF["tau_10"]),
             H1_fixed_vs_random = exp(m.fixed.H1$logmarginal - m.random.H1$logmarginal))

  results <- list(meta = c(fixed.H0 = list(logml = logml.fixed.H0),
                           sapply(metas[2:4], resampling, resample=resample)),
                  logmarginal = sapply(metas, function(meta) meta$logmarginal),
                  BF = BF,
                  bma = bma,
                  estimates = NA)
  class(results) <- "meta_bma"

  #-------------------------------------------------------------------------------
  # estimates + model-averaged estimates
  #-------------------------------------------------------------------------------

  # calculate weighted posterior of effects by combining
  # mcmc samples from fixed and rand proportional to BF_fixed_vs_random_unres

  if(estimate == "jags"){
    nFixed <- round(sample* BF$H1_fixed_vs_random/
                      (BF$H1_fixed_vs_random + 1))
    mixedVec <- c(resampling(m.fixed.H1, nFixed)$samples,
                  resampling(m.random.H1, sample - nFixed)$samples)

    ests <- rbind(averaged = summary_ests(mixedVec),
                  fixed = summary_ests(m.fixed.H1, "d.fixed"),
                  random = summary_ests(m.random.H1, "d.random"))
  }else{
    bbd = bounds_prior(data_list$prior.d, "d.random")
    results$estimates <-
      rbind(averaged = stats_density(posterior(results), bbd[1], bbd[2]),
            fixed = stats_density(posterior(m.fixed.H1), bbd[1], bbd[2]),
            random = stats_density(posterior(m.random.H1), bbd[1], bbd[2]))
  }

  return(results)
}
