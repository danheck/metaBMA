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
#' d1 <- meta_default(logOR, SE, study, towels,
#'                    field = "psych", effect = "logOR")
#' d1
#' plot_forest(d1)
#' }
#' @seealso \code{\link{meta_bma}}, \code{\link{plot_default}}
#' @template ref_gronau2017
#' @export
meta_default <- function(y, se, labels, data,
                         field = "psychology", effect = "ttest", ...){

  def <- get_default(field, effect)
  dl <- data_list("random", y = y, se = se, labels = labels, data = data,
                  args = as.list(match.call()))
  res <- meta_bma(y = "y", se = "se", labels = "labels", data = dl,
                  d = def$d,  tau = def$tau,
                  ...)
  res$default <- c("field" = field, "effect" = effect)
  res
}


get_default <- function (field, effect){
  field <- match.arg(field, c("psychology", "medicine"))
  effect <- match.arg(effect, c("ttest", "logOR", "corr"))

  switch(field,
         #################################### PSYCHOLOGY
         "psychology" = {
           switch(effect,
                  "ttest" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(mu = 0, sigma = .5, nu = 1), lower = 0)
                  },
                  "logOR" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(mu = 0, sigma = .5, nu = 1), lower = 0)
                  },
                  "corr" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(mu = 0, sigma = .5, nu = 1), lower = 0)
                  },
                  stop("'effect' not supported."))
         },

         #################################### MEDICINE
         "medicine" = {
           switch(effect,
                  "ttest" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(mu = 0, sigma = .5, nu = 1), lower = 0)
                  },
                  "logOR" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(mu = 0, sigma = .5, nu = 1), lower = 0)
                  },
                  "corr" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(mu = 0, sigma = .5, nu = 1), lower = 0)
                  },
                  stop("'effect' not supported."))
         },
         stop("'field' not supported."))

  list(d = d, tau = tau)
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
plot_default <- function(field, effect, ...){
  mfrow <- par()$mfrow
  par(mfrow = c(1,2))
  def <- get_default(field, effect)
  plot(def$d, xlab = "Mean effect (d)", ...)
  plot(def$tau, xlab = "Standard deviation (tau)", ...)
  par(mfrow = mfrow)
}
