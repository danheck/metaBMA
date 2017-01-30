#' Model Averaging for Meta-Analysis
#'
#' Bayesian model averaging for four meta-analysis models: Fixed- vs. random-effects and H0 (\eqn{d=0}) vs. H1 (e.g., \eqn{d>0}).
#'
#' @param prior prior probabilities over models (possibly unnormalized) in the order \code{c(fixed.H0, fixed.H1, random.H0, random.H1)}. For instance, if we expect fixed effects to be two times as likely as random effects and H0 and H1 to be equally likely: \code{prior = c(2,2,1,1)}
#' @param y mean in each study
#' @param V variance in each study
#' @param d type of prior for mean effect \eqn{d} (see \code{\link{log_prior}})
#' @param d.par prior parameters for \eqn{d}
#' @param tau type of prior for study-variance \eqn{\tau} in random-effects meta-analysis (i.e., the variance of d across studies; see \code{\link{log_prior}})
#' @param tau.par prior parameters for \eqn{\tau}
#' @param marginal how to integrate marginal likelihood (\code{"bridge"} or \code{"integrate"})
#' @param estimate whether to use JAGS or analytical solutions to estimate parameters
# @param sample number of samples in JAGS after burn-in and thinning (see \code{\link[runjags]{run.jags}})
# @param burnin number of burn-in samples in JAGS
# @param n.chains number of chains in JAGS
# @param thin thinning rate in JAGS
#' @param targetLength xxx
#' @param resample how many samples should be resampled and stored from JAGS (e.g., for plotting; with replacement).
#' @param progress wheter to print progress of fitting the models
#' @param ... arguments passed to \link[runjags]{run.jags} (e.g., MCMC parameters such as \code{sample}, \code{burnin}, \code{n.chains}, \code{thin} or \code{method="parallel"})
#' @export
meta_bma <- function(y,
                     V,
                     d = "norm",
                     d.par = c(0, .3),
                     tau = "halfcauchy",
                     tau.par=.5,
                     prior = c(1,1,1,1),
                     marginal = "bridge",
                     estimate = "jags",
                     targetLength = 80000,
                     resample = 5000,
                     progress = TRUE,
                     ...){

  # sample=100000,
  # burnin=2000,
  # n.chains=3,
  # thin=20,
  # values for tau prior
  data_list <- list(y = y, V = V,
                    model="averaged",
                    prior.d = list(name = d, par = d.par),
                    prior.tau = list(name = tau, par = tau.par),
                    prior.models = prior/sum(prior))

  #-------------------------------------------------------------------------------
  # run meta-analyses
  #-------------------------------------------------------------------------------

  logml.fixed.H0 <- loglik_fixed_H0(data_list)

  if(progress) cat("### Fixed effects H1 ###\n")
  m.fixed.H1 <- meta_fixed(y, V, d=d, d.par = d.par, ...)

  if(progress) cat("### Random effects H0 ###\n")
  m.random.H0 <- meta_random(y, V, d = "0", tau = tau, tau.par=tau.par, ...)

  if(progress) cat("### Random effects H1 ###\n")
  m.random.H1 <- meta_random(y, V, d = d, d.par = d.par,
                             tau = tau, tau.par=tau.par, ...)

  #-------------------------------------------------------------------------------
  # store results
  #-------------------------------------------------------------------------------

  metas <- list(fixed.H0 = list(logmarginal = logml.fixed.H0),
                fixed.H1 = m.fixed.H1,
                random.H0 = m.random.H0,
                random.H1 = m.random.H1)
  logml <- sapply(metas, function(meta) meta$logmarginal)

  # inclusion probability (model-averaged posterior probability of effect)
  bma <- bma(logml[c("fixed.H0",  "fixed.H1",
                     "random.H0",  "random.H1")],
             include = c(2,4),
             prior = prior)

  BFs <- list(BF_10_fixed = exp(m.fixed.H1$logmarginal - logml.fixed.H0),
              BF_10_random = exp(m.random.H1$logmarginal - m.random.H0$logmarginal),
              BF_10_averaged = bma$BF.inclusion,
              BF_fixed_vs_random = exp(m.fixed.H1$logmarginal - m.random.H1$logmarginal))

  #-------------------------------------------------------------------------------
  # estimates + model-averaged estimates
  #-------------------------------------------------------------------------------

  # calculate weighted posterior of effects by combining
  # mcmc samples from fixed and rand proportional to BF_fixed_vs_random_unres

  nFixed <- round(targetLength* BFs$BF_fixed_vs_random/
                    (BFs$BF_fixed_vs_random + 1))
  nRand <- targetLength - nFixed
  mixedVec <- c(resampling(m.fixed.H1, nFixed)$samples,
                resampling(m.random.H1, nRand)$samples)

  ests <- rbind(fixed = summary_ests(m.fixed.H1, "d.fixed"),
                random = summary_ests(m.random.H1, "d.random"),
                averaged = summary_ests(mixedVec))

  results <- list(meta = c(fixed.H0 = list(logml = logml.fixed.H0),
                           sapply(metas[2:4], resampling, resample=resample)),
                  logmarginal = logml,
                  BF = BFs,
                  bma = bma,
                  estimates = ests)
  class(results) <- "meta_bma"

  return(results)
}
