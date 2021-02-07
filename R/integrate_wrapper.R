integrate_wrapper <- function(data, d, tau,
                              rel.tol = .Machine$double.eps^0.5,
                              ratio.tol = .0001) {

  # shifting the posterior to have the mode (approximately) at zero
  weights <- 1 / data$SE^2
  shift <- sum(data$y * weights) / sum(weights)

  if (data$model == "fixed" && attr(d, "family") != "0") {
    ####### fixed H1
    target <- function(x) {
      post_fixed(x + shift, data = data, prior = d)
    }
    bnd <- bounds_prior(d) - shift
  } else if (data$model == "random" && attr(d, "family") == "0") {
    ####### random H0
    target <- function(x) {
      post_random(x + shift, d = 0, data = data, prior_d = d, prior_tau = tau)
    }
    bnd <- bounds_prior(tau) - shift
  } else if (data$model == "random") {
    ####### random H1
    target <- function(x) {
      post_random_d(x + shift, data = data, prior_d = d, prior_tau = tau, rel.tol = rel.tol)
    }
    bnd <- bounds_prior(d) - shift
  } else {
    return(NA)
  }

  # integrate twice to ensure stability
  scale <- NA
  scale_int <- integrate(target,
    lower = bnd[1], upper = bnd[2], rel.tol = rel.tol,
    subdivisions = 1000L, stop.on.error = FALSE
  )
  if (scale_int$message != "OK" ||
    scale_int$value == 0 ||
    scale_int$abs.error / scale_int$value > ratio.tol) {
    scale_int <- integrate(target,
      lower = bnd[1], upper = bnd[2],
      rel.tol = .Machine$double.eps, subdivisions = 1000L,
      stop.on.error = FALSE
    )
    if (!scale_int$message %in% c("OK", "roundoff error was detected")) {
      warning("integrate log-marginal likelihood: ", scale_int$message)
    }
  }
  if (scale_int$value == 0) {
    warning("Posterior density could not be integrated numerically (integral = 0).")
  } else {
    scale <- scale_int$value
  }

  # re-integrate for stability
  integral <- 1
  if (!is.na(scale) && scale != 0) {
    integral_int <- integrate(function(x) target(x) / scale,
      lower = bnd[1], upper = bnd[2],
      rel.tol = rel.tol, subdivisions = 1000L,
      stop.on.error = FALSE
    )
    if (integral_int$message == "OK") {
      integral <- integral_int$value
    }
    # warning("Second call of integrate: ", integral_int$message)
  }
  log(integral * scale)
}
