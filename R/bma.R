#' Bayesian Model Averaging
#'
#' Model averaging of meta-analysis models according to their posterior model probability.
#'
#' @param meta list of meta-analysis models (fitted via \code{\link{meta_random}} or \code{\link{meta_fixed}})
#' @inheritParams inclusion
#' @inheritParams meta_bma
#' @param parameter eiher the mean effect \code{"d"} or the heterogeneity across studies \code{"tau"}
#'
#' @examples
#' fix1 <- meta_fixed(towels$logOR, towels$SE, towels$study,
#'                    d = "halfnorm", d.par = c(mean=0, sd=.2))
#' fix2 <- meta_fixed(towels$logOR, towels$SE, towels$study,
#'                    d = "beta", d.par = c(alpha=1, beta=1))
#' fix3 <- meta_fixed(towels$logOR, towels$SE, towels$study,
#'                    d = "triangular",
#'                    d.par = c(min=0, peak=.3, max=1))
#'
#' averaged <- bma(list(Halfnormal = fix1, Uniform = fix2,
#'                      Triangular = fix3))
#' averaged
#' plot_posterior(averaged)
#' plot_forest(averaged)
#' @export
bma <- function (meta,
                 prior = 1,
                 parameter = "d",
                 summarize = "integrate"){

  classes <- sapply(meta, class) %in% c("meta_fixed", "meta_random")
  if (!is.list(meta) || !all(classes))
    stop ("'meta' must be a list of meta-analysis models \n",
          "       (fitted with meta_fixed and meta_random).")

  sel.prior <- paste0("prior.", parameter)
  sel.post <- paste0("posterior.", parameter)
  logml <- sapply(meta, function (meta) meta$logmarginal)
  if (!is.null(names(meta)))
    names(logml) <- names(meta)
  incl <- inclusion(logml, prior = prior)

  res_bma <- list("meta" = meta,
                  "logmarginal" = logml,
                  "prior.models" = incl$prior,
                  "posterior.models" = incl$posterior,
                  "prior.d" = sapply(meta, function(x) x[[sel.prior]]),
                  "posterior.d" = sapply(meta, function(x) x[[sel.post]]),
                  "estimates" = NULL)
  class(res_bma) <- "meta_bma"

  # BMA: weighted posterior of effects
  res_bma$posterior.d <- posterior(res_bma)
  if (summarize %in% c("integrate", "jags")){
    ests <- t(sapply(meta, function(x) x$estimates[parameter,]))
    res_bma$estimates <- rbind("Averaged" = stats_density(res_bma$posterior.d),
                               ests)
    if (!is.null(names(meta)))
      rownames(res_bma$estimates)[1+1:length(meta)] <- names(meta)
  }

  res_bma$BF <- outer(exp(res_bma$logmarginal),
                      exp(res_bma$logmarginal), "/")
  return (res_bma)
}
