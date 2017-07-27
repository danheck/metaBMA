#' Defaults for Model Averaging in Meta-Analysis
#'
#' Wrapper with default prior for Bayesian meta-analysis based on a literature review.
#' Currently, the same default is used in all cases.
#'
#' @inheritParams meta_bma
#' @param field either\code{"psychology"} or \code{"medicine"}
#'     (uses partial matching, so \code{"p"} and "\code{"m"} are sufficient)
#' @param effect the type of effect size: either means (\code{"ttest"}),
#'     log-odds ratios (\code{"logOR"}) or
#'     (Fisher's z-transformed) correlations (\code{"corr"}) (also uses partial matching)
#' @param ... further arguments passed to \code{\link{meta_bma}}
#'
#' @details
#' Default prior distributions can be plotted using \code{\link{plot_default}}.
#'
#' For \code{field = "psychology"}, the following defaults are used:
#' \itemize{
#' \item \code{effect = "ttest"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "logOR"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "corr"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' }
#'
#' For \code{field = "medicine"}, the following defaults are used:
#' \itemize{
#' \item \code{effect = "ttest"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "logOR"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "corr"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' }
#'
#' @examples
#' \dontrun{
#' data(towels)
#' d1 <- meta_default(towels$logOR, towels$SE, towels$study,
#'                    field = "psych", effect = "logOR",
#'                    sample = 5000)
#' d1
#' plot_forest(d1)
#' }
#' @seealso \code{\link{meta_bma}}, \code{\link{plot_default}}
#' @template ref_gronau2017
#' @export
meta_default <- function(y,
                         SE,
                         labels = NULL,
                         field = "psychology",
                         effect = "ttest",
                         ...){
  def <- get_default(field, effect)

  res <- meta_bma(y = y, SE = SE, labels = labels,
           d = def$d, d.par = def$d.par,
           tau = def$tau, tau.par = def$tau.par,
           ...)
  res$default <- c(field = field, effect = effect)
  return(res)
}


get_default <- function(field,
                        effect){
  field <- match.arg(field, c("psychology", "medicine"))
  effect <- match.arg(effect, c("ttest", "logOR", "corr"))

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

  def <- list(d = d, d.par = d.par,
              tau = tau, tau.par = tau.par)
  # class(def) <- "default"
  return(def)
}


#' Plot Default Priors
#'
#' Plots default priors for the mean effect \eqn{d} and the standard deviation of effects \eqn{tau}.
#' @inheritParams meta_default
#' @param ... further arguments passed to \code{\link[graphics]{plot}} (e.g., \code{from}, \code{to})
#' @examples
#' plot_default("psychology", "ttest", 0, 2)
#' plot_default("medicine", "logOR", 0, 2)
#' @seealso \code{\link{meta_default}} for details on standard priors.
#' @export
plot_default <- function(field,
                         effect,
                         ...){
  mfrow <- par()$mfrow
  par(mfrow = c(1,2))
  def <- get_default(field, effect)
  plot(prior(def$d, def$d.par), xlab = "Mean effect (d)", ...)
  plot(prior(def$tau, def$tau.par), xlab = "Standard deviation (tau)", ...)
  par(mfrow = mfrow)
}
