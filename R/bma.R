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
#' data(towels)
#' fix1 <- meta_fixed(logOR, SE, study, towels,
#'                    d = prior("norm", c(mean=0, sd=.2), lower=0))
#' fix2 <- meta_fixed(logOR, SE, study, towels,
#'                    d = prior("beta", c(shape1=1, shape2=1), upper = 2))
#' fix3 <- meta_fixed(logOR, SE, study, towels,
#'                    d = prior("custom", function(x) x^2, 0, 1))
#'
#' averaged <- bma(list(Halfnormal = fix1, Uniform = fix2,
#'                      Quadratic = fix3))
#' averaged
#' plot_posterior(averaged)
#' plot_forest(averaged, mar = c(4.5,20,4,.3))
#' @export
bma <- function (meta, prior = 1, parameter = "d", summarize = "integrate", ci = .95,
                 rel.tol = .Machine$double.eps^0.5){

  classes <- sapply(meta, class) %in% c("meta_fixed", "meta_random")
  if (!is.list(meta) || !all(classes))
    stop ("'meta' must be a list of meta-analysis models \n",
          "       (fitted with meta_fixed and meta_random).")
  if (is.null(names(meta)))
    names(meta) <- paste0("meta", seq_along(meta))
  check_data_identical(meta)

  sel.prior <- paste0("prior_", parameter)
  sel.post <- paste0("posterior_", parameter)
  logml <- sapply(meta, "[[", "logml")
  incl <- inclusion(logml, prior = prior)

  res_bma <- list("meta" = meta,
                  "logml" = logml,
                  "prior_models" = incl$prior,
                  "posterior_models" = incl$posterior,
                  "prior_d" = lapply(meta, function(x) x[[sel.prior]]),
                  "posterior_d" = lapply(meta, function(x) x[[sel.post]]),
                  "estimates" = NULL)
  class(res_bma) <- "meta_bma"

  # BMA: weighted posterior of effects
  summarize <- match.arg(summarize, c("stan", "integrate"))
  ests <- do.call("rbind", lapply(meta, function(x) x$estimates[parameter,]))

  res_bma[[paste0("posterior_", parameter)]] <-
    posterior(res_bma, parameter, rel.tol = rel.tol)
  if (summarize == "integrate"){
    res_bma$estimates <- rbind("Averaged" = summary_integrate(res_bma$posterior_d,
                                                              ci = ci, rel.tol = rel.tol),
                               ests)
  } else if (summarize == "stan"){
    samples <- lapply(meta, function(m) extract(m$stanfit, parameter)[[parameter]])
    maxiter <- max(sapply(samples, length))
    nn <- round(maxiter * incl$posterior)
    avg_samples <- unlist(mapply(sample, x = samples, size = nn,
                                 MoreArgs = list(replace = TRUE)))
    res_bma$estimates <- rbind("Averaged" = summary_samples(avg_samples), ests)
    # res_bma[[paste0("posterior_", parameter)]] <-
    #   posterior_logspline(avg_samples, parameter, meta[[1]][[paste0("prior_", parameter)]])
  }

  res_bma$BF <- outer(exp(res_bma$logml), exp(res_bma$logml), "/")
  names(dimnames(res_bma$BF)) <- c("numerator", "denominator")
  res_bma
}
