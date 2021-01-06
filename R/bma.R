#' Bayesian Model Averaging
#'
#' Model averaging for different meta-analysis models (e.g., random-effects or
#' fixed-effects with different priors) based on the posterior model
#' probability.
#'
#' @param meta list of meta-analysis models (fitted via
#'   \code{\link{meta_random}} or \code{\link{meta_fixed}})
#' @inheritParams inclusion
#' @inheritParams meta_bma
#' @param parameter either the mean effect \code{"d"} or the heterogeneity \code{"tau"}
#'   (i.e., the across-study standard deviation of population effect sizes).
#'
#' @examples
#' # Note: The following example optimizes speed (for CRAN checks).
#' #       The settings are not suitable for actual data analysis!
#'
#' # model averaging for fixed and random effects
#' data(towels)
#' fixed <- meta_fixed(logOR, SE, study, towels)
#' random <- meta_random(logOR, SE, study, towels, rel.tol = .01, iter = 500)
#'
#' averaged <- bma(list("fixed" = fixed, "random" = random))
#' averaged
#' plot_posterior(averaged)
#' plot_forest(averaged, mar = c(4.5,20,4,.3))
#' @export
bma <- function(meta, prior = 1, parameter = "d", summarize = "integrate", ci = .95,
                rel.tol = .Machine$double.eps^0.5){

  # stopifnot(parameter %in% c("d", "tau"))
  if (parameter == "tau")
    stop("Model averaging for heterogeneity 'tau' currently not supported.")

  classes <- sapply(meta, class) %in% c("meta_fixed", "meta_random")
  if (!is.list(meta) || !all(classes))
    stop ("'meta' must be a list of meta-analysis models \n",
          "       (fitted with meta_fixed and meta_random).")
  if (is.null(names(meta)))
    names(meta) <- paste0("meta", seq_along(meta))
  check_data_identical(meta)

  # select models that contain parameter of interest
  # (current issue with tau-averaging: random_H0 not fitted by meta_bma!)
  select_models <- sapply(meta, function(mm) parameter %in% rownames(mm$estimates))

  sel.prior <- paste0("prior_", parameter)
  sel.post <- paste0("posterior_", parameter)
  logml <- unlist(lapply(meta, "[[", "logml"))
  names(logml) <- gsub("random.random", "random",
                       gsub("fixed.fixed", "fixed", names(logml)))

  # get model prior/posterior probabilities (inclusion BF not used):
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
  summ_list <- lapply(meta[select_models], function(x) x$estimates[parameter,])
  ests <- do.call("rbind", summ_list)

  # meaningful names for models in posterior summary table: (issue: may break compatibility with JASP)
  # rownames(ests) <- paste0(substr(sapply(meta, class), 6, 99), "_H1.", names(meta))[select_models]

  # averaged posterior density: weighted average of densities
  summarize <- match.arg(summarize, c("stan", "integrate"))
  res_bma[[paste0("posterior_", parameter)]] <-
    posterior(meta = res_bma, parameter = parameter, summarize = summarize, rel.tol = rel.tol)
  ests_avg <- summary_integrate(res_bma[[paste0("posterior_", parameter)]],
                                ci = ci, rel.tol = rel.tol)

  ################ NOT WORKING WELL: SAMPLING-BASED MODEL AVERAGING
  ################ ----> averaged posterior may not be a convex combination of fixed-/random effects
  # if (summarize == "integrate"){
  # } else {
  #   samples <- lapply(meta[select_models], function(m) extract(m$stanfit, parameter)[[parameter]])
  #   maxiter <- sapply(samples, length)
  #
  #   sel_H1 <- paste0(names(meta), "_H1")
  #   weights <- incl$posterior[sel_H1] / sum(incl$posterior[sel_H1])
  #
  #   # M = (fictional) total number of samples for weighted posterior
  #   # M * weights = maxiter
  #   M <- floor(min(maxiter / weights))
  #   nn <- round(M * weights)
  #
  #   # nn <- round(maxiter * incl$posterior[sel_H1] / max(incl$posterior[sel_H1]))
  #   avg_samples <- unlist(mapply(sample, x = samples, size = nn,
  #                                MoreArgs = list(replace = FALSE)))
  #   ests_avg <- summary_samples(avg_samples)
  #   res_bma[[paste0("posterior_", parameter)]] <-
  #     posterior_logspline(avg_samples, parameter, meta[[select_models[1]]][[paste0("prior_", parameter)]])
  # }
  #
  # curve(posterior(meta = res_bma, parameter = parameter, summarize = summarize, rel.tol = rel.tol)(x), -.2, .2, lwd = 2)
  # curve(res_bma[[paste0("posterior_", parameter)]] (x), add=T)
  # curve(res_bma$meta$fixed$posterior_d(x), add=T, col=2)
  # curve(res_bma$meta$random$posterior_d(x), add=T, col=4)

  res_bma$estimates <- rbind("averaged" = ests_avg, ests)
  res_bma$BF <- make_BF(res_bma$logml)
  res_bma
}
