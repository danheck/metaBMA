
# construct posterior density function
posterior <- function (meta, parameter = "d", summarize = "integrate",
                       rel.tol = .Machine$double.eps^0.5){

  if (class(meta) == "meta_bma"){
    if (parameter != "d")
      stop("bma currently. only working for parameter='d' ")
    # average across posterior densities:
    if (length(meta$posterior_models) != length(meta$posterior_d))
      models <- grep("H1",names(meta$posterior_models),
                     value  = TRUE, fixed = TRUE)
    else
      models <- seq_along(meta)
    weights <- meta$posterior_models[models]
    weights <- weights / sum(weights)
    dpost_bma <- function (d)
      sapply(d, function (dd){
        sum(weights*sapply(meta$posterior_d, function(dp) dp(dd)))
      })
    lb <- sapply(meta$prior_d, attr, which = "lower")
    ub <- sapply(meta$prior_d, attr, which = "upper")
    attr(dpost_bma, "lower") <- min(lb)
    attr(dpost_bma, "upper") <- max(ub)
    attr(dpost_bma, "model") <- "bma"
    return(check_posterior(dpost_bma, meta, parameter, rel.tol))
  }

  dpost <- NULL

  if (length(parameter) == 1){

    if (summarize == "integrate"){
      if (parameter == "d"){
        if (meta$model == "fixed")
          dpost <- function (x)
            post_fixed(d = x, data = meta$data, meta$prior_d, rel.tol = rel.tol)/exp(meta$logml)
        else if (meta$model == "random")
          dpost <- function (x)
            post_random_d(d = x, data = meta$data, prior_d = meta$prior_d,
                          prior_tau = meta$prior_tau, rel.tol = rel.tol)/exp(meta$logml)

      } else if (parameter == "tau"){
        if (meta$model == "random")
          dpost <- function (x)
            post_random_tau(tau = x, data = meta$data, prior_d = meta$prior_d,
                            prior_tau = meta$prior_tau, rel.tol = rel.tol)/exp(meta$logml)
      }
    }

    prior <- meta[[paste0("prior_", parameter)]]
    if (is.null(dpost))
      dpost <- posterior_logspline(meta$stanfit, parameter, prior)  # JZS!
    attr(dpost, "lower") <- attr(prior, "lower")
    attr(dpost, "upper") <- attr(prior, "upper")
    attr(dpost, "model") <- meta$model
    attr(dpost, "param") <- parameter
    return(check_posterior(dpost, meta, parameter, rel.tol))

  } else  if (meta$model == "random"){
    dpost_tau_d <- function (tau, d)
      post_random(tau = tau, d = d, data = meta$data, prior_d = meta$prior_d,
                  prior_tau = meta$prior_tau, rel.tol = rel.tol)/exp(meta$logml)
    attr(dpost_tau_d, "lower") <- c(attr(meta$prior_tau, "lower"),
                                    attr(meta$prior_d, "lower"))
    attr(dpost_tau_d, "upper") <- c(attr(meta$prior_tau, "upper"),
                                    attr(meta$prior_d, "upper"))
    attr(dpost_tau_d, "model") <- meta$model
    return(check_posterior(dpost_tau_d, meta, parameter, rel.tol))
  }


  return(NULL)  # 2D- kernel density for JZS required!
}
