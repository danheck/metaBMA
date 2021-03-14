#' Defaults for Model Averaging in Meta-Analysis
#'
#' Wrapper with default prior for Bayesian meta-analysis.
#' Since version 0.6.6, the default priors for Cohen's d have been changed from a
#' normal distribution with scale=0.3 to a Cauchy distribution with scale=0.707.
#' Moreover, scale adjustments were implemented when using Fisher's z or log odds-ratios.
#'
#' @inheritParams meta_bma
#' @param field either\code{"psychology"} or \code{"medicine"}
#' @param effect the type of effect size used in the meta-analysis: either
#'     Cohen's d (\code{"d"}),
#'     Fisher's z-transformed correlation (\code{"z"}),
#'     or log odds ratios (\code{"logOR"}).
#' @param ... further arguments passed to \code{\link{meta_bma}}
#'
#' @details
#' The prior distribution depends on the scale of the effect size that is used in
#' the meta-analysis (Cohen's d, Fisher's z, or log odds ratio). To ensure that
#' the results are comparable when transforming between different effect sizes,
#' it is necessary to adjust the scale of the prior distributions.
#'
#' \itemize{
#' \item The distribution of Fisher's z is approximately half as wide as the
#' distribution of Cohen's d and hence the prior scale parameter is divided by two.
#' \item The distribution of the log odds ratio is approximately
#'  \code{pi / sqrt(3) = 1.81} times as wide as the distribution of Cohen's d.
#' Hence, the prior scale parameter is doubled by this factor.
#' }
#'
#' For \code{field = "psychology"}, this results in the following defaults:
#' \itemize{
#' \item \code{effect = "d"} (Cohen's d): Cauchy distribution with scale=0.707 on the overall
#'     effect size (parameter d) and inverse gamma distribution with shape=1 and
#'     scale=0.15 on the standard deviation of effect sizes across studies (parameter tau).
#' \item \code{effect = "z"} (Fisher's z): Cauchy distribution with scale=0.354 on d and
#'     inverse gamma with shape=1 and scale=0.075 on tau.
#' \item \code{effect = "logOR"} (log odds ratio): Cauchy distribution with scale=1.283 on d and
#'     inverse gamma with shape=1 and scale=0.272 on tau.
#' }
#'
#' Currently, the same priors are used when specifying \code{field = "medicine"}.
#'
#' Default prior distributions can be plotted using \code{\link{plot_default}}.
#'
#' @template ref_borenstein2009
#'
#' @examples
#' \donttest{
#' data(towels)
#' set.seed(123)
#' md <- meta_default(logOR, SE, study, towels,
#'   field = "psychology", effect = "logOR"
#' )
#' md
#' plot_forest(md)
#' }
#' @seealso \code{\link{meta_bma}}, \code{\link{plot_default}}
#' @template ref_gronau2017
#' @export
meta_default <- function(y, SE, labels, data,
                         field = "psychology", effect = "d", ...) {
  def <- get_default(field, effect)
  dl <- data_list("random",
                  y = y, SE = SE, labels = labels, data = data,
                  args = as.list(match.call())
  )
  res <- meta_bma(
    y = "y", SE = "SE", labels = "labels", data = dl,
    d = def$d, tau = def$tau,
    ...
  )
  res$default <- c("field" = field, "effect" = effect)
  res
}


get_default <- function(field, effect) {
  field <- match.arg(field, c("psychology", "medicine"))
  effect <- match.arg(effect, c("d", "z", "logOR", "ttest", "corr"))

  if (effect %in% c("ttest", "corr")) {
    stop(
      "The options effect='ttest' and effect='corr' are deprecated for metaBMA (>0.3.9).\n",
      "The option effect='r' is deprecated for metaBMA (>=0.6.6)",
      "\n  The type of effect size must be one of the following:",
      "\n    effect=c('d', 'z', 'logOR')",
      "\n  (see ?meta_default)"
    )
    # effect <- ifelse(effect == "ttest", "d", "z")
  }

  switch(field,
         #################################### PSYCHOLOGY
         "psychology" = {
           switch(effect,
                  "d" = {
                    d <- prior("cauchy", c(location = 0, scale = sqrt(1/2)))
                    tau <- prior("invgamma", c(shape = 1, scale = 0.15))
                  },
                  "z" = {
                    d <- prior("cauchy", c(location = 0, scale = sqrt(1/2) / 2))
                    tau <- prior("invgamma", c(shape = 1, scale = 0.15 / 2))
                  },
                  "logOR" = {
                    d <- prior("cauchy", c(location = 0, scale = sqrt(1/2)*pi/sqrt(3)))
                    tau <- prior("invgamma", c(shape = 1, scale = 0.15 * pi/sqrt(3)))
                  },
                  stop("'effect' not supported.")
           )
         },

         #################################### MEDICINE
         "medicine" = {
           switch(effect,
                  "d" = {
                    d <- prior("cauchy", c(location = 0, scale = sqrt(1/2)))
                    tau <- prior("invgamma", c(shape = 1, scale = 0.15))
                  },
                  "z" = {
                    d <- prior("cauchy", c(location = 0, scale = sqrt(1/2) / 2))
                    tau <- prior("invgamma", c(shape = 1, scale = 0.15 / 2))
                  },
                  "logOR" = {
                    d <- prior("cauchy", c(location = 0, scale = sqrt(1/2)*pi/sqrt(3)))
                    tau <- prior("invgamma", c(shape = 1, scale = 0.15 * pi/sqrt(3)))
                  },
                  stop("'effect' not supported.")
           )
         },
         stop("'field' not supported.")
  )

  list("d" = d, "tau" = tau)
}


#' Plot Default Priors
#'
#' Plots default priors for the mean effect \eqn{d} and the standard deviation of effects \eqn{tau}.
#' @inheritParams meta_default
#' @param ... further arguments passed to \code{\link[graphics]{plot}} (e.g., \code{from}, \code{to})
#' @examples
#' plot_default(field = "psychology", effect = "d")
#' @seealso \code{\link{meta_default}} for details on standard priors.
#' @export
plot_default <- function(field = "psychology", effect = "d", ...) {
  mfrow <- par()$mfrow
  par(mfrow = c(1, 2))
  def <- get_default(field, effect)
  plot(def$d, xlab = "Average effect (d)", ...)
  plot(def$tau, xlab = "Across-study standard deviation (tau)", ...)
  par(mfrow = mfrow)
}
