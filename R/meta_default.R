#' Defaults for Model Averaging in Meta-Analysis
#'
#' Wrapper with default prior for Bayesian meta-analysis based on a literature review.
#'
#' @inheritParams meta_bma
#' @param field either\code{"psychology"} or \code{"medicine"}
#' @param effect the type of effect size: either means (\code{"ttest"}), log-odds ratios (\code{"logOR"}) or (Fisher's z-transformed) correlations (\code{"corr"})
#' @param ... further arguments passed to \code{\link{meta_bma}}
#'
#' @details
#' Default prior distributions can be plotted using \code{\link{plot_default}}.
#'
#' For \code{field = "psychology"}, the following defaults are used:
#' \itemize{
#' \item \code{effect = "ttest"}: Half-normal with SD=0.3 on mean effect and half-Cauchy with scale=.5 on variance of effects.
#' \item \code{effect = "logOR"}: Half-normal with SD=0.3 on mean effect and half-Cauchy with scale=.5 on variance of effects.
#' \item \code{effect = "corr"}: Half-normal with SD=0.3 on mean effect and half-Cauchy with scale=.5 on variance of effects.
#' }
#'
#' For \code{field = "medicine"}, the following defaults are used:
#' \itemize{
#' \item \code{effect = "ttest"}: Half-normal with SD=0.3 on mean effect and half-Cauchy with scale=.5 on variance of effects.
#' \item \code{effect = "logOR"}: Half-normal with SD=0.3 on mean effect and half-Cauchy with scale=.5 on variance of effects.
#' \item \code{effect = "corr"}: Half-normal with SD=0.3 on mean effect and half-Cauchy with scale=.5 on variance of effects.
#' }
#'
#' @seealso \link{meta_bma}
#' @export
meta_default <- function(y,
                         V,
                         field = "psychology",
                         effect = "ttest",
                         ...){
  def <- get_default(field, effect)

  meta_bma(y = y, V = V,
           d = def$d$name, d.par = def$d$par,
           tau = def$tau$name, tau.par = def$tau$par,
           ...)
}


get_default <- function(field,
                        effect){
  switch(field,
         #################################### PSYCHOLOGY
         "psychology" = {
           switch(effect,
                  "ttest" = {
                    d = "halfnorm"
                    d.par <- c(mean = 0, sd = .3)
                    tau  <- "halfcauchy"
                    tau.par <- c(scale = .5)
                  },
                  "logOR" = {
                    d = "halfnorm"
                    d.par <- c(mean = 0, sd = .3)
                    tau  <- "halfcauchy"
                    tau.par <- c(scale = .5)
                  },
                  "corr" = {
                    d = "halfnorm"
                    d.par <- c(mean = 0, sd = .3)
                    tau  <- "halfcauchy"
                    tau.par <- c(scale = .5)
                  },
                  stop("'effect' not supported."))
         },

         #################################### MEDICINE
         "medicine" = {
           switch(effect,
                  "ttest" = {
                    d = "halfnorm"
                    d.par <- c(mean = 0, sd = .3)
                    tau  <- "halfcauchy"
                    tau.par <- c(scale = .5)
                  },
                  "logOR" = {
                    d = "halfnorm"
                    d.par <- c(mean = 0, sd = .3)
                    tau  <- "halfcauchy"
                    tau.par <- c(scale = .5)
                  },
                  "corr" = {
                    d = "halfnorm"
                    d.par <- c(mean = 0, sd = .3)
                    tau  <- "halfcauchy"
                    tau.par <- c(scale = .5)
                  },
                  stop("'effect' not supported."))
         },
         stop("'field' not supported."))

  def <- list(d = list(name = d, par = d.par),
              tau = list(name = tau, par = tau.par))
  return(def)
}


#' Plot Default Priors
#'
#' Plots default priors for the mean effect \eqn{d} and the variance of effects \eqn{tau}.
#' @inheritParams meta_default
#' @param ... further arguments passed to \code{\link[graphics]{plot}} (e.g., \code{from}, \code{to})
#' @examples
#' plot_default("psychology", "ttest", 0, 2)
#' plot_default("medicine", "logOR", 0, 2)
#' @export
plot_default <- function(field,
                         effect,
                         ...){
  mfrow <- par()$mfrow
  par(mfrow = c(1,2))
  def <- get_default(field, effect)
  plot(prior(def$d), xlab = "Mean effect (d)", ...)
  plot(prior(def$tau), xlab = "Variance (tau)", ...)
  par(mfrow = mfrow)
}
