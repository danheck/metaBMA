
# construct posterior density function
posterior <- function(meta, parameter = "d", summarize = "integrate",
                      rel.tol = .Machine$double.eps^0.5) {
  if (class(meta) == "meta_bma") {
    if (parameter != "d") {
      stop("bma currently. only working for parameter='d' ")
    }

    # labels for prior/posterior
    prior_par <- paste0("prior_", parameter)
    posterior_par <- paste0("posterior_", parameter)

    # # select relevant models that have posterior for parameter [TODO: method for tau:random_H0/random_H1]
    # if (parameter == "d"){
    #   models <- sapply(meta, function(mm) parameter %in% grep(mm$model))
    # } else if parameter == "tau"){
    #   select_models <- sapply(meta, function(mm) parameter %in% rownames(mm$estimates))
    # } else {
    #   stop("The argument 'parameter' must be either 'd' or 'tau'.")
    # }

    # cope with the fact that H0/H1 models are represented differently internally:
    if (length(meta$posterior_models) != length(meta[[posterior_par]])) {
      models <- grep("H1", names(meta$posterior_models), value = TRUE, fixed = TRUE)
    } else {
      models <- seq_along(meta)
    }
    weights <- meta$posterior_models[models]
    weights <- weights / sum(weights)

    # average across posterior densities:
    dpost_bma <- function(d) {
      sapply(d, function(dd) {
        sum(weights * sapply(meta[[posterior_par]], function(dp) dp(dd)))
      })
    }
    lb <- sapply(meta[[prior_par]], attr, which = "lower")
    ub <- sapply(meta[[prior_par]], attr, which = "upper")
    attr(dpost_bma, "lower") <- min(lb)
    attr(dpost_bma, "upper") <- max(ub)
    attr(dpost_bma, "model") <- "bma"

    return(check_posterior(dpost_bma, meta, parameter, rel.tol))
  }

  dpost <- NULL

  if (length(parameter) == 1) {
    if (summarize == "integrate") {
      if (parameter == "d") {
        if (meta$model == "fixed") {
          dpost <- function(x) {
            post_fixed(d = x, data = meta$data, meta$prior_d, rel.tol = rel.tol) / exp(meta$logml)
          }
        } else if (meta$model == "random") {
          dpost <- function(x) {
            post_random_d(
              d = x, data = meta$data, prior_d = meta$prior_d,
              prior_tau = meta$prior_tau, rel.tol = rel.tol
            ) / exp(meta$logml)
          }
        }
      } else if (parameter == "tau") {
        if (meta$model == "random") {
          dpost <- function(x) {
            post_random_tau(
              tau = x, data = meta$data, prior_d = meta$prior_d,
              prior_tau = meta$prior_tau, rel.tol = rel.tol
            ) / exp(meta$logml)
          }
        }
      }
    }

    prior <- meta[[paste0("prior_", parameter)]]
    if (is.null(dpost)) {
      dpost <- posterior_logspline(meta$stanfit, parameter, prior)
    } # TODO: JZS beta slope parameters!
    attr(dpost, "lower") <- attr(prior, "lower")
    attr(dpost, "upper") <- attr(prior, "upper")
    attr(dpost, "model") <- meta$model
    attr(dpost, "param") <- parameter
    return(check_posterior(dpost, meta, parameter, rel.tol))
  } else if (meta$model == "random") {
    dpost_tau_d <- function(tau, d) {
      post_random(
        tau = tau, d = d, data = meta$data, prior_d = meta$prior_d,
        prior_tau = meta$prior_tau, rel.tol = rel.tol
      ) / exp(meta$logml)
    }
    attr(dpost_tau_d, "lower") <- c(
      attr(meta$prior_tau, "lower"),
      attr(meta$prior_d, "lower")
    )
    attr(dpost_tau_d, "upper") <- c(
      attr(meta$prior_tau, "upper"),
      attr(meta$prior_d, "upper")
    )
    attr(dpost_tau_d, "model") <- meta$model
    return(check_posterior(dpost_tau_d, meta, parameter, rel.tol))
  }


  return(NULL) # 2D- kernel density for JZS required!
}
