#' Bayesian Random-Effects Meta-Analysis
#'
#' Bayesian meta-analysis assuming that the effect size \eqn{d} varies across
#' studies with standard deviation \eqn{\tau} (i.e., a random-effects model).
#'
#' @inheritParams meta_bma
# ' @param truncation whether the study-specific random-effects should be
# '   truncated. The default \code{truncation=FALSE} corresponds to a standard
# '   random-effects meta-analysis in which study specific effects can be both
# '   negative and positive (even if the prior for the overall effect size
# '   \code{d} is truncated). If \code{truncation=TRUE}, the truncation
# '   boundaries of the study-specific random-effects are defined via
# '   \code{d=prior(..., lower=0, upper=Inf)}.
#'
#' @examples
#' ### Bayesian Random-Effects Meta-Analysis
#' # Note: The following code optimizes speed (for CRAN checks).
#' #       The settings are not suitable for actual data analysis!
#'
#' data(towels)
#' mr <- meta_random(logOR, SE, study, data = towels,
#'                   d = prior("norm", c(mean=0, sd=.3)),
#'                   tau = prior("t", c(location=0, scale=.5, nu=1), lower=0),
#'                   rel.tol = .Machine$double.eps^.2)  # speed!
#'                   # (no summary: only for CRAN checks)
#' mr
#' plot_posterior(mr)
#' @export
meta_random <- function(y, SE, labels, data,
                        d = prior("norm", c(mean=0, sd=.3), lower=0),
                        tau  = prior("invgamma", c(shape = 1, scale = 0.15)),
                        rscale_contin = 1/2, rscale_discrete = sqrt(2)/2, centering = TRUE,
                        logml = "integrate", summarize = "stan", ci = .95,
                        rel.tol = .Machine$double.eps^.3,
                        logml_iter = 3000, ...){

  data_list <- data_list(model = "random", y = y, SE = SE, labels = labels,
                         data = data, args = as.list(match.call()))

  d <- check_prior(d)
  tau <- check_prior(tau, 0)
  meta <- list("model" = data_list$model,
               "data" = data_list,
               "prior_d" = d,
               "prior_tau" = tau,
               "jzs" = list("rscale_contin" = rscale_contin,
                            "rscale_discrete" = rscale_discrete,
                            "centering" = centering),
               "posterior_d" = NULL,
               "posterior_tau" = NULL,
               "logml" = NA,
               "BF" = NULL,
               "estimates" = NULL)
  class(meta) <- "meta_random"

  logml <- match.arg(logml, c("integrate", "stan"))
  if (attr(d, "family") %in% priors_stan()){

    # estimate random effects
    data_list2 <- data_list
    data_list2$model <- paste0(data_list$model, "_dstudy")
    meta$stanfit_dstudy <- meta_stan(data_list2, d = d, tau = tau, jzs = meta$jzs,
                                     pars = c("d", "tau", "dstudy"), ...)

    # get samples for bridge sampling / BF
    if (logml == "stan"){
      args <- c(list("data_list" = data_list, "d" = d, "tau" = tau, "jzs" = meta$jzs),
                list(...))
      args$iter <- logml_iter
      meta$stanfit <- do.call("meta_stan", args)
    }

  } else {
    if (logml == "stan" || summarize == "stan")
      stop("Prior not supported by Stan models.",
         "\nUse logml='integrate' and summarize='integrate'")
  }

  if (logml == "integrate"){
    meta$logml <- integrate_wrapper(data_list, d, tau, rel.tol = rel.tol)
  }
  meta <- meta_bridge_sampling(meta, logml, ...)

  meta$posterior_d <- posterior(meta, "d", rel.tol = rel.tol)
  meta$posterior_tau <- posterior(meta, "tau", rel.tol = rel.tol)

  meta$estimates <- summary_meta(meta, summarize)
  meta$BF <- c("d_10" = meta$prior_d(0) / meta$posterior_d(0),
               "tau_10" = meta$prior_tau(0) / meta$posterior_tau(0))
  meta
}

