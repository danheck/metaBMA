#' Bayesian Fixed-Effects Meta-Analysis
#'
#' Runs a Bayesian meta-analysis assuming that the mean effect \eqn{d} in each
#' study is identical (i.e., a fixed-effects analysis).
#'
#' @inheritParams meta_bma
#' @inheritParams meta_random
#'
#' @examples
#' ### Bayesian Fixed-Effects Meta-Analysis (H1: d>0)
#' data(towels)
#' mf <- meta_fixed(logOR, SE, study,
#'   data = towels,
#'   d = prior("norm", c(mean = 0, sd = .3), lower = 0)
#' )
#' mf
#' plot_posterior(mf)
#' plot_forest(mf)
#' @export
meta_fixed <- function(
    y,
    SE,
    labels,
    data,
    d = prior("cauchy", c(location = 0, scale = 0.707)),
    rscale_contin = 1 / 2,
    rscale_discrete = 0.707,
    centering = TRUE,
    logml = "integrate",
    summarize = "integrate",
    ci = .95,
    rel.tol = .Machine$double.eps^.3,
    silent_stan = TRUE,
    ...
) {

  if ("tau" %in% names(list(...))) {
    warning("The fixed-effects model assumes tau=0. Hence, a prior distribution cannot be specified.")
  }

  logml <- match.arg(logml, c("integrate", "stan"))
  summarize <- match.arg(summarize, c("integrate", "stan"))
  data_list <- data_list("fixed",
    y = y, SE = SE, labels = labels, data = data,
    args = as.list(match.call())[-1]
  )
  jzs <- grepl("jzs", data_list$model)

  check_deprecated(list(...)) # error: backwards compatibility
  d <- check_prior(d)
  tau <- prior("0", c(), label = "tau")

  meta <- list(
    "model" = data_list$model,
    "data" = data_list, "prior_d" = d, "prior_tau" = tau,
    "jzs" = list(
      rscale_contin = rscale_contin,
      rscale_discrete = rscale_discrete,
      centering = centering
    ),
    "posterior_d" = NA, "posterior_tau" = tau,
    "logml" = NA, "BF" = NULL, "estimates" = NULL
  )
  class(meta) <- "meta_fixed"

  if (attr(d, "family") %in% priors_stan() && attr(d, "family") != "0" || jzs) {
    meta$stanfit <- meta_stan(data_list, d = d, jzs = meta$jzs, silent_stan = silent_stan, ...)
  }

  if (logml == "integrate" ||
    attr(d, "family") == "0" ||
    !attr(d, "family") %in% priors_stan()) {
    meta$logml <- integrate_wrapper(data_list, d, rel.tol = rel.tol)
  }
  meta <- meta_bridge_sampling(meta, logml, ...)

  # not for fixed_jzs
  meta$posterior_d <- posterior(meta, "d", rel.tol = rel.tol)
  summ <- summary_meta(meta, summarize, ci = ci, rel.tol = rel.tol)
  meta$estimates <- summ[c("d", grep("beta", rownames(summ), value = TRUE)), , drop = FALSE]

  logml_fixedH0 <- NA
  # analytical/numerical integration: only without JZS moderator structure!
  if (data_list$model == "fixed") {
    logml_fixedH0 <- loglik_fixed_H0(data_list)
  }

  # Savage-Dickey (if JZS present):
  if (is.na(logml_fixedH0)) {
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
        "\n  d=prior('halfcauchy', c(scale=0.707))"
      )
    } else {
      bf_fixed_10 <- d(0) / meta$posterior_d(0)
      logml_fixedH0 <- -log(bf_fixed_10) + meta$logml
    }
  }

  meta$logml <- c("fixed_H0" = logml_fixedH0, "fixed_H1" = meta$logml)
  meta$BF <- make_BF(meta$logml)
  inclusion <- inclusion(meta$logml, include = c(2), prior = c(1, 1))
  meta$prior_models <- inclusion$prior
  meta$posterior_models <- inclusion$posterior
  meta
}
