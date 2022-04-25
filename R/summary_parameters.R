summary_meta <- function(meta, summarize = "integrate", ci = .95,
                         rel.tol = .Machine$double.eps^.3) {
  summarize <- match.arg(summarize, c("integrate", "stan"))
  estimates <- NULL
  if (summarize == "integrate") {
    if (meta$model == "fixed") {
      estimates <- rbind("d" = summary_integrate(meta$posterior_d,
        rel.tol = rel.tol, ci = ci
      ))
    } else if (meta$model == "random") {
      estimates <- rbind(
        "d" = summary_integrate(meta$posterior_d,
          rel.tol = rel.tol, ci = ci
        ),
        "tau" = summary_integrate(meta$posterior_tau,
          rel.tol = rel.tol, ci = ci
        )
      )
    } else {
      warning(
        "summarize = 'integrate' not supported for model: ", meta$model,
        "\n  (summary of MCMC/Stan samples is reported instead)"
      )
    }
  }

  # fallback: Stan samples
  if (is.null(estimates)) {
    if (is.null(meta$stanfit)) {
      warning(
        "summarize = 'stan' not supported for model: ", meta$model,
        "\n  (MCMC/Stan samples are missing)"
      )
    } else {
      estimates <- summary_stanfit(meta$stanfit, ci = ci)
      idx <- grep("beta", rownames(estimates))
      if (length(idx) > 0) {
        X <- add_jzs(meta$data, meta$jzs)$X
        rownames(estimates)[idx] <- paste0("beta_", colnames(X))
      }
    }
  }

  estimates
}

# x: vector
summary_samples <- function(x, ci = .95) {
  probs <- c((1 - ci) / 2, .5, 1 - (1 - ci) / 2)
  s <- c(mean(x), sd(x), quantile(x, probs))
  names(s) <- c("mean", "sd", paste0(round(probs * 100, digits = 1), "%"))

  hpd <- c(HPDinterval(mcmc(x), ci))
  names(hpd) <- paste0("hpd", round(ci * 100, 2), c("_lower", "_upper"))

  ##### n_eff / Rhat
  # coda:::gelman.diag(as.mcmc(x))
  # coda:::effectiveSize(as.mcmc(x))
  # => not enough information to use in bma: random subset of samples
  # => information about chains and order is lost!
  c(s, hpd, "n_eff" = NA, "Rhat" = NA)
}

#' @importFrom coda varnames
#' @importFrom rstan As.mcmc.list
summary_stanfit <- function(stanfit, ci = .95) {
  # stan summary: cbind(summ[,- c(2,7,8),drop = FALSE], hpd)[sel,,drop = FALSE]
  samples <- As.mcmc.list(stanfit)
  mcmc <- do.call("rbind", samples)
  summ <- t(apply(mcmc, 2, summary_samples, ci = ci))
  sel <- rownames(summ) != "lp__" & !grepl("g[", rownames(summ), fixed = TRUE)

  # add convergence diagnostics
  summ[sel, "n_eff"] <- round(summary(stanfit)$summary[sel, "n_eff"], 1)
  summ[sel, "Rhat"] <- round(summary(stanfit)$summary[sel, "Rhat"], 3)
  summ[sel, , drop = FALSE]
}

summary_integrate <- function(density, lb = NULL, ub = NULL, ci = .95,
                              rel.tol = .Machine$double.eps^0.8) {
  if (inherits(density, c("prior", "posterior"))) {
    lb <- attr(density, "lower")
    ub <- attr(density, "upper")
  }
  if (lb == ub) {
    return(NULL)
  }

  mean <- NA
  try(mean <- integrate(function(x) x * density(x),
    lb, ub,
    rel.tol = rel.tol
  )$value, silent = TRUE)

  SD <- NA
  try(
    {
      variance <- integrate(function(x) (x - mean)^2 * density(x),
        lb, ub,
        rel.tol = rel.tol
      )$value
      SD <- sqrt(variance)
    },
    silent = TRUE
  )

  ################ quantiles
  probs <- c((1 - ci) / 2, .50, 1 - (1 - ci) / 2)
  qq <- rep(NA, 3)
  try(
    {
      tmp <- function(q, p) {
        p - integrate(density, lb, q, rel.tol = rel.tol)$value
      }
      for (i in c(3, 5, 10, 20, 100, 500, 1000, 5000, 10000)) {
        interval <- c(
          max(lb + 1e-30, mean - i * SD),
          min(mean + i * SD, ub - 1e-30)
        )
        q.diff <- sapply(interval, tmp, p = probs)
        if (all(q.diff[, 1] > 0) && all(q.diff[, 2] < 0)) {
          break
        }
      }
      qq <- sapply(probs, function(pp) uniroot(tmp, interval, p = pp)$root)
    },
    silent = TRUE
  )

  ################ compute HPD
  hpd <- c("hpd95_lower" = NA, "hpd95_upper" = NA)
  try(
    {
      xx <- seq(max(lb, qq[1] - i * SD), min(ub, qq[3] + i * SD), length.out = 201)
      dx <- sapply(xx, density) * diff(xx[1:2])
      px <- cumsum(dx)
      # use monotonic spline function
      qdens <- splinefun(px, xx, method = "monoH.FC", ties = "mean")
      # plot(px ,xx, col = "gray") ; curve(qdens, add =TRUE)

      length.interval <- function(start) {
        qdens(start + ci) - qdens(start)
      }
      oo <- optim((1 - ci) / 2, length.interval,
        method = "L-BFGS-B",
        lower = min(px), upper = max(px) - ci
      )
      hpd <- c(
        "hpd95_lower" = qdens(oo$par),
        "hpd95_upper" = qdens(oo$par + ci)
      )
      names(hpd) <- paste0("hpd", round(ci * 100, 2), c("_lower", "_upper"))
    },
    silent = TRUE
  )

  warn <- paste(
    "Some posterior statistics (mean, SD, etc) could not be computed\n",
    " for parameter", attr(density, "label"), "using numerical integration.\n",
    " Estimates based on MCMC/Stan samples will be shown instead\n",
    "(for high precision, refit the models with:  iter=10000,warmup=500)"
  )
  if (any(is.na(c(mean, qq, SD, hpd)))) {
    warning(warn)
  }

  names(qq) <- paste0(round(probs * 100, digits = 1), "%")
  c("mean" = mean, "sd" = SD, qq, hpd, "n_eff" = NA, "Rhat" = NA)
}
