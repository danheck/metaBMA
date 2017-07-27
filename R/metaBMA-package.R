#' metaBMA: Bayesian Model Averaging for Random and Fixed Effects Meta-Analysis
#'
#' Fixed-effects meta-analyses assume that the effect size \eqn{d} is identical in all studies. In contrast, random-effects meta-analyses assume that effects vary according to a normal distribution with mean \eqn{d} and standard deviation \eqn{\tau}. Both models can be compared in a Bayesian framework by assuming specific prior distribution for \eqn{d} and \eqn{\tau} (see \code{\link{prior}}). Given the posterior model probabilities, the evidence for or against an effect (i.e., whether \eqn{d = 0}) and the evidence for or against random effects can be evaluated (i.e., whether \eqn{\tau = 0}). By using Bayesian model averaging, both tests can be performed by integrating over the other model. This allows to test whether an effect exists while accounting for uncertainty whether study heterogeneity exists (so-called inclusion Bayes factors).
#'
#' The most general functions in \code{metaBMA} are \code{\link{meta_bma}} and \code{\link{meta_default}}, which fit random- and fixed-effects models, compute the inclusion Bayes factor for the presence of an effect and the averaged posterior distribution of the mean effect \eqn{d} (which accounts for uncertainty regarding study heterogeneity). Prior distributions can be specified and plotted using the function \code{\link{prior}}.
#'
#' Moreover, \code{\link{meta_fixed}} and \code{\link{meta_random}} fit a single meta-analysis models. The model-specific posteriors for \eqn{d} can be averaged by \code{\link{bma}} and inclusion Bayes factors be computed by \code{\link{inclusion}}.
#'
#' Results can be visualized with the functions \code{\link{plot_posterior}}, which compares the prior and posterior density for a fitted meta-analysis, and \code{\link{plot_forest}}, which plots study and overall effect sizes.
#'
#' For more details, see the vignette: \code{vignette("metaBMA")}
#'
#' @author Heck, D. W., Gronau, Q. F., & Wagenmakers, E.-J.
#' @docType package
#' @importFrom logspline logspline dlogspline
#' @importFrom runjags run.jags runjags.options combine.mcmc
#' @importFrom mvtnorm rmvnorm dmvnorm
#' @import stats
#' @importFrom coda spectrum0.ar HPDinterval as.mcmc varnames mcmc
#' @importFrom LaplacesDemon dhalfcauchy dst rst rhalft rhalfcauchy
#' @import graphics
#' @importFrom grDevices adjustcolor colors
# ' @importFrom Matrix nearPD
# ' @useDynLib metaBMA
# ' @importFrom Brobdingnag as.brob
#' @references
#' Heck, D. W., Gronau, Q. F., & Wagenmakers, E.-J. (2017). metaBMA: Bayesian model averaging for random and fixed effects meta-analysis. Retrieved from https://github.com/danheck/metaBMA. doi:10.5281/zenodo.835494
#' @template ref_gronau2017
"_PACKAGE"


