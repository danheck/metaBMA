#' Bayesian Random-Effects Meta-Analysis
#'
#' Bayesian meta-analysis assuming that the effect size \eqn{d} varies across
#' studies with standard deviation \eqn{\tau} (i.e., a random-effects model).
#'
#' @inheritParams meta_bma
#'
#' @examples
#' \donttest{
#' ### Bayesian Random-Effects Meta-Analysis (H1: d>0)
#' data(towels)
#' set.seed(123)
#' mr <- meta_random(logOR, SE, study,
#'   data = towels,
#'   d = prior("norm", c(mean = 0, sd = .3), lower = 0),
#'   tau = prior("invgamma", c(shape = 1, scale = 0.15))
#' )
#' mr
#' plot_posterior(mr)
#' }
#' @export
meta_random <- function(y, SE, labels, data,
                        d = prior("cauchy", c(location = 0, scale = 0.707)),
                        tau = prior("invgamma", c(shape = 1, scale = 0.15)),
                        rscale_contin = 0.5, rscale_discrete = 0.707, centering = TRUE,
                        logml = "integrate", summarize = "stan", ci = .95,
                        rel.tol = .Machine$double.eps^.3,
                        logml_iter = 5000, silent_stan = TRUE, ...) {

  check_deprecated(list(...)) # error: backwards compatibility
  logml <- match.arg(logml, c("integrate", "stan"))
  summarize <- match.arg(summarize, c("integrate", "stan"))
  data_list <- data_list(
    model = "random", y = y, SE = SE, labels = labels,
    data = data, args = as.list(match.call())[-1]
  )

  d <- check_prior(d)
  tau <- check_prior(tau, 0)
  meta <- list(
    "model" = data_list$model,
    "data" = data_list,
    "prior_d" = d,
    "prior_tau" = tau,
    "jzs" = list(
      "rscale_contin" = rscale_contin,
      "rscale_discrete" = rscale_discrete,
      "centering" = centering
    ),
    "posterior_d" = NULL,
    "posterior_tau" = NULL,
    "logml" = NA,
    "BF" = NULL,
    "estimates" = NULL
  )
  class(meta) <- "meta_random"

  if (attr(d, "family") %in% priors_stan() &&
    attr(tau, "family") %in% priors_stan()) {

    # estimate random effects
    data_list2 <- data_list
    data_list2$model <- paste0(data_list$model, "_dstudy")
    jzs <- grepl("jzs", data_list2$model)
    pars <- c("d", "tau", "dstudy", c("beta")[jzs])
    meta$stanfit_dstudy <- meta_stan(data_list2,
      d = d, tau = tau, jzs = meta$jzs,
      pars = pars, silent_stan = silent_stan, ...
    )

    # get samples for bridge sampling / BF
    if (logml == "stan" || jzs) {
      args <- c(
        list("data_list" = data_list, "d" = d, "tau" = tau, "jzs" = meta$jzs),
        list(...)
      )
      args$iter <- logml_iter
      meta$stanfit <- do.call("meta_stan", args)
    }
  } else {
    if (logml == "stan" || summarize == "stan") {
      stop(
        "Prior not supported by Stan models.",
        "\nUse logml='integrate' and summarize='integrate'"
      )
    }
  }

  if (logml == "integrate") {
    meta$logml <- integrate_wrapper(data_list, d, tau, rel.tol = rel.tol)
  }
  meta <- meta_bridge_sampling(meta, logml, ...)

  meta$posterior_d <- posterior(meta, "d", summarize, rel.tol = rel.tol)
  meta$posterior_tau <- posterior(meta, "tau", summarize, rel.tol = rel.tol)
  summ <- summary_meta(meta, summarize, ci = ci, rel.tol = rel.tol)
  meta$estimates <- summ[c("d", "tau", grep("beta", rownames(summ), value = TRUE)), , drop = FALSE]

  logml_randomH0 <- NA
  # analytical/numerical integration: only without JZS moderator structure!
  if (data_list$model == "random") {
    logml_randomH0 <- integrate_wrapper(data_list, d = prior("0", "d"), tau, rel.tol = rel.tol)
  }

  # Savage-Dickey (if JZS present or numerical integration failed):
  if (is.na(logml_randomH0)) {
    n_samples <- length(extract(meta$stanfit, "d")[["d"]])
    if (n_samples < 10000) {
      warning(
        "If discrete/continuous moderators are specified, the Bayes factor is computed",
        "\nbased on the Savage-Dickey density ratio. For high precision, this requires",
        "\na larger number of samples for estimation as specified via:\n    iter=10000"
      )
    }

    if (meta$prior_d(0) == 0) {
      warning(
        "Savage-Dickey density ratio can only be used if the prior on the",
        "\noverall effect size d is strictly positive at zero. For example, use:",
        "\n  d=prior('halfnorm', c(sd=0.3))"
      )
    } else {
      bf_random_10 <- meta$prior_d(0) / meta$posterior_d(0)
      logml_randomH0 <- -log(bf_random_10) + meta$logml
    }
  }

  meta$logml <- c("random_H0" = logml_randomH0, "random_H1" = meta$logml)
  meta$BF <- make_BF(meta$logml)
  inclusion <- inclusion(meta$logml, include = c(2), prior = c(1, 1))
  meta$prior_models <- inclusion$prior
  meta$posterior_models <- inclusion$posterior
  meta
}
