#' Defaults for Model Averaging in Meta-Analysis
#'
#' Wrapper with default prior for Bayesian meta-analysis based on a literature review.
#' Currently, the same default is used in all cases.
#'
#' @inheritParams meta_bma
#' @param field either\code{"psychology"} or \code{"medicine"}
#'     (uses partial matching, so \code{"p"} and "\code{"m"} are sufficient)
#' @param effect the type of effect size: either
#'     Cohen's d (\code{"d"}),
#'     Pearson correlations (\code{"r"}),
#'     Fisher's z-transformed correlations (\code{"z"}),
#'     or log-odds ratios (\code{"logOR"}).
#' @param ... further arguments passed to \code{\link{meta_bma}}
#'
#' @details
#' Default prior distributions can be plotted using \code{\link{plot_default}}.
#'
#' For \code{field = "psychology"}, the following defaults are used:
#' \itemize{
#' \item \code{effect = "d"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "r"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "z"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "logOR"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' }
#'
#' For \code{field = "medicine"}, the following defaults are used:
#' \itemize{
#' \item \code{effect = "d"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "r"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "z"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' \item \code{effect = "logOR"}: Half-normal with SD=0.3 on mean effect and
#'     half-Cauchy with scale=.5 on standard deviation of effects.
#' }
#'
#' @examples
#' # Note: The following example optimizes speed (for CRAN checks).
#' #       The settings are not suitable for actual data analysis!
#'
#' data(towels)
#' set.seed(123)
#' md <- meta_default(logOR, SE, study, towels,
#'                    field = "psych", effect = "logOR",
#'                    rel.tol=.Machine$double.eps^.15, iter=1000)
#' md
#' plot_forest(md)
#' @seealso \code{\link{meta_bma}}, \code{\link{plot_default}}
#' @template ref_gronau2017
#' @export
meta_default <- function(y, SE, labels, data,
                         field = "psychology", effect = "d", ...){

  def <- get_default(field, effect)
  dl <- data_list("random", y = y, SE = SE, labels = labels, data = data,
                  args = as.list(match.call()))
  res <- meta_bma(y = "y", SE = "SE", labels = "labels", data = dl,
                  d = def$d, tau = def$tau,
                  ...)
  res$default <- c("field" = field, "effect" = effect)
  res
}


get_default <- function (field, effect){
  field <- match.arg(field, c("psychology", "medicine"))
  effect <- match.arg(effect, c("d", "r", "z", "logOR", "ttest", "corr"))

  if (effect %in% c("ttest", "corr")){
    warning("The options effect='ttest' and effect='corr' are deprecated for metaBMA (>0.3.9).",
            "\n  The type of effect size is now labeled as:",
            "\n    effect=c('d', 'r', 'z', 'logOR')",
            "\n  (see ?meta_default)")
    effect <- ifelse(effect == "ttest", "d", "z")
  }

  switch(field,
         #################################### PSYCHOLOGY
         "psychology" = {
           switch(effect,
                  "d" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(location = 0, scale = .5, nu = 1), lower = 0)
                  },
                  "r" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(location = 0, scale = .5, nu = 1), lower = 0)
                  },
                  "z" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(location = 0, scale = .5, nu = 1), lower = 0)
                  },
                  "logOR" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(location = 0, scale = .5, nu = 1), lower = 0)
                  },
                  stop("'effect' not supported."))
         },

         #################################### MEDICINE
         "medicine" = {
           switch(effect,
                  "d" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(location = 0, scale = .5, nu = 1), lower = 0)
                  },
                  "r" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(location = 0, scale = .5, nu = 1), lower = 0)
                  },
                  "z" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(location = 0, scale = .5, nu = 1), lower = 0)
                  },
                  "logOR" = {
                    d = prior("norm", c(mean = 0, sd = .3), lower = 0)
                    tau  <- prior("t", c(location = 0, scale = .5, nu = 1), lower = 0)
                  },
                  stop("'effect' not supported."))
         },
         stop("'field' not supported."))

  list("d" = d, "tau" = tau)
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
plot_default <- function(field = "psychology", effect = "d", ...){
  mfrow <- par()$mfrow
  par(mfrow = c(1,2))
  def <- get_default(field, effect)
  plot(def$d, xlab = "Average effect (d)", ...)
  plot(def$tau, xlab = "Across-study standard deviation (tau)", ...)
  par(mfrow = mfrow)
}
