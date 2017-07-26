#' Model Averaging for Meta-Analysis
#'
#' Fits random- and fixed-effects meta-anayses and performs Bayesian model averaging for H1 (d != 0) vs. H0 (d = 0).
#'
#' Bayesian model averaging for four meta-analysis models: Fixed- vs. random-effects and H0 (\eqn{d=0}) vs. H1 (e.g., \eqn{d>0}).
#'
#' @param prior prior probabilities over models (possibly unnormalized) in the order \code{c(fixed.H0, fixed.H1, random.H0, random.H1)}. For instance, if we expect fixed effects to be two times as likely as random effects and H0 and H1 to be equally likely: \code{prior = c(2,2,1,1)}
#' @param y mean in each study
#' @param SE standard error in each study
#' @param labels optional: character values with study labels
#' @param d type of prior for mean effect \eqn{d} (see \code{\link{prior}})
#' @param d.par prior parameters for \eqn{d}
#' @param tau type of prior for standard deviation of study effects \eqn{\tau} in random-effects meta-analysis (i.e., the SD of d across studies; see \code{\link{prior}})
#' @param tau.par prior parameters for \eqn{\tau}
# ' @param marginal how to integrate marginal likelihood (\code{"bridge"} or \code{"integrate"})
#' @param sample number of samples in JAGS after burn-in and thinning (see \code{\link[runjags]{run.jags}}). Samples are used to get posterior  estimates for each study effect (which will show shrinkage). Only works for priors defined in \code{\link{prior}}.
#' @param summarize whether and to compute parameter summaries (mean, median, SD, 95\% quantile interval, HPD interval). If \code{summarize = "integrate"}, numerical integration is used  (which is precise but can require some seconds of computing time), \code{summarize = "jags"} summarizes the JAGS samples, and \code{summarize = "none"} suppresses parameter summaries.
#' @param ... arguments passed to \link[runjags]{run.jags} (e.g., MCMC parameters such as \code{sample}, \code{burnin}, \code{n.chains}, \code{thin} or \code{method="parallel"})
#'
#' @examples
#' \dontrun{
#' data(towels)
#' mb <- meta_bma(towels$logOR, towels$SE, towels$study,
#'                d = "norm", d.par = c(0,.3),
#'                tau = "halfcauchy", tau.par = .5)
#' mb
#' plot_posterior(mb, "d")
#' plot_forest(mb)
#' }
#' @seealso \link{meta_default}, \link{meta_fixed}, \link{meta_random}
#' @export
meta_bma <- function (y,
                      SE,
                      labels = NULL,
                      d = "norm",
                      d.par = c(0, .3),
                      tau = "halfcauchy",
                      tau.par=.5,
                      prior = c(1,1,1,1),
                      sample = 10000,
                      summarize = "integrate",
                      ...){

  data_list <- data_list(model = "bma", y = y, SE = SE,
                         labels = labels, d = d, d.par = d.par,
                         tau = tau, tau.par = tau.par, prior = prior)

  # fit meta-analysis models
  m.fixed.H1 <- meta_fixed(y, SE, labels, d = d, d.par = d.par, sample = sample,
                           summarize = summarize, ...)
  m.random.H1 <- meta_random(y, SE, labels, d = d, d.par = d.par,
                             tau = tau, tau.par=tau.par, sample = sample,
                             summarize = summarize, ...)

  # model averaging
  meta <- list(`Fixed Effects` = m.fixed.H1,
               `Random Effects` = m.random.H1)
  meta_bma <- bma(meta, prior = prior[c(2, 4)], parameter = "d",
             summarize = summarize)

  # inclusion bayes factors etc.
  logml.random.H0 <- m.random.H1$logmarginal - log(m.random.H1$BF["d_10"])
  logml <- c("fixed.H0" = loglik_fixed_H0(data_list),
             "fixed.H1" = m.fixed.H1$logmarginal,
             "random.H0" = matrix(logml.random.H0),
             "random.H1" = m.random.H1$logmarginal)
  meta_bma$logmarginal <- logml
  meta_bma$inclusion <- inclusion(logml, include = c(2,4), prior = prior)
  meta_bma$prior.models <- meta_bma$inclusion$prior
  meta_bma$posterior.models <- meta_bma$inclusion$posterior

  meta_bma$BF <- list("d_10_fixed" = c(m.fixed.H1$BF["d_10"]),
                 "d_10_random" = c(m.random.H1$BF["d_10"]),
                 "d_10_averaged" = meta_bma$inclusion$incl.BF,
                 "tau_10_random" = c(m.random.H1$BF["tau_10"])
                 # "H1_fixed_vs_random" = exp(m.fixed.H1$logmarginal -
                 #                            m.random.H1$logmarginal)
                 )
  return (meta_bma)
}
