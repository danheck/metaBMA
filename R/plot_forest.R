#' Forest Plot for Meta-Analysis
#'
#' Plots estimated effect sizes for all studies.
#'
#' @param meta fitted meta-analysis model
#' @param from lower limit of the x-axis
#' @param to upper limit of the x-axis
#' @param shrinked which meta-analysis model should be used to show (shrinked)
#'     estimates of the study effect sizes. The name must match the corresponding
#'     name in the list \code{meta}. Can be suppressed by \code{shrinked = ""}
#' @param mar margin of the plot in the order \code{c(bottom, left, top, right)}
#'     (see \code{\link[graphics]{par}})
#' @param cex.axis size of the y-axis annotation for the labels of studies.
#' @param summary character vector with two values: first, either \code{"mean"}
#'     or \code{"50\%"}; and second, either highest-probability-density interval \code{"hpd"}
#'     or the Bayesian credibility interval \code{"bci"}.
#' @param ... arguments passed to \code{\link[graphics]{plot}} (e.g., \code{from}, \code{to})
#'
#' @seealso \link{meta_bma}, \link{meta_fixed}, \link{meta_random}
#' @examples
#' data(towels)
#' mf <- meta_fixed(logOR, SE, study, towels)
#' plot_forest(mf, mar = c(4.5, 20, 4, .2), xlab = "Log Odds Ratio")
#' @export
plot_forest <- function(
    meta,
    from,
    to,
    shrinked = "random",
    summary = c("mean", "hpd"),
    mar = c(4.5, 12, 4, .3),
    cex.axis = 1,
    ...
) {
  par(mar = mar)
  UseMethod("plot_forest", meta)
}


#' @export
plot_forest.meta_fixed <- function(
    meta,
    from,
    to,
    shrinked = "random",
    summary = c("mean", "hpd"),
    mar = c(4.5, 12, 4, .3),
    cex.axis = 1,
    main = "Fixed-Effects Meta-Analysis",
    ...
) {
  plot_forest.default(meta, from, to,
    summary = summary,
    shrinked = "",
    main = main,
    cex.axis = cex.axis, ...
  )
  axis(2, -1, "Total", las = 1, tick = FALSE, cex.axis = cex.axis)
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}


#' @export
plot_forest.meta_random <- function(
    meta,
    from,
    to,
    shrinked = "random",
    summary = c("mean", "hpd"),
    mar = c(4.5, 12, 4, .3),
    cex.axis = 1,
    main = "Random-Effects Meta-Analysis",
    ...
) {
  plot_forest.default(meta,
    from = from, to = to, summary = summary,
    shrinked = shrinked, cex.axis = cex.axis,
    main = main, ...
  )
  axis(2, -1, "Total", las = 1, tick = FALSE, cex.axis = cex.axis)
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}


#' @export
plot_forest.meta_bma <- function(
    meta,
    from,
    to,
    shrinked = "random",
    summary = c("mean", "hpd"),
    mar = c(4.5, 12, 4, .3),
    cex.axis = 1,
    main = "Meta-Analysis with Model-Averaging",
    ...
) {
  meta$data <- meta$meta[[1]]$data
  plot_forest.default(meta,
    from = from, to = to, summary = summary,
    shrinked = shrinked, cex.axis = cex.axis,
    main = main, ...
  )
  labels <- rownames(meta$estimates)
  axis(2, -seq_along(labels), labels, las = 1, tick = TRUE, cex.axis = cex.axis)
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}


# ' @rdname plot_posterior
#' @export
plot_forest.default <- function(
    meta,
    from,
    to,
    shrinked = "random",
    summary = c("mean", "hpd"),
    mar = c(4.5, 12, 4, .3),
    cex.axis = 1,
    main = "Meta-Analysis",
    ...
) {
  ci <- .95

  n.studies <- length(meta$data$y)
  sel <- grep("d", rownames(meta$estimates))
  if ("averaged" %in% rownames(meta$estimates)) {
    sel <- 1:nrow(meta$estimates)
  }
  n.ests <- length(sel)
  lower <- meta$data$y - qnorm((ci + 1) / 2) * meta$data$SE
  upper <- meta$data$y + qnorm((ci + 1) / 2) * meta$data$SE

  stanfit_dstudy <- ss <- NULL
  if (inherits(meta, "meta_bma")) {
    stanfit_dstudy <- meta$meta[[shrinked]]$stanfit_dstudy
  } else if (inherits(meta, "meta_random") && shrinked != "") {
    stanfit_dstudy <- meta$stanfit_dstudy
  }
  if (!is.null(stanfit_dstudy)) {
    try(ss <- as.mcmc(rstan::extract(stanfit_dstudy, "dstudy")$dstudy), silent = TRUE)
  }

  if (!is.null(ss)) {
    summ <- summary(ss)
    # sel.d <- paste0("d[",seq_len(n.studies),"]")
    if (summary[1] == "Mean") {
      rand.est <- summ$statistics[, "Mean"]
    } else {
      rand.est <- summ$quantiles[, "50%"]
    }
    if (summary[2] == "HPD") {
      hpd <- HPDinterval(ss[, ], ci)
      rand.low <- hpd[, 1]
      rand.up <- hpd[, 2]
    } else {
      rand.low <- summ$quantiles[, "2.5%"]
      rand.up <- summ$quantiles[, "97.5%"]
    }
  }
  if (missing(from)) from <- max(attr(meta$prior_d, "lower"), -1)
  if (missing(to)) to <- min(attr(meta$prior_d, "upper"), 1)
  xxx <- pretty(c(from, to, lower, upper))

  Effect <- -1000
  plot(
    x = Effect,
    y = -100,
    xlim = range(xxx),
    yaxt = "n",
    ylab = "",
    las = 1,
    ylim = c(-n.ests, n.studies),
    bty = "n",
    main = main,
    ...
  )
  abline(v = 0, col = "darkgray", lty = "dashed")
  axis(2, n.studies:1, meta$data$labels, las = 1, cex.axis = cex.axis)

  segments(x0 = lower, x1 = upper, n.studies:1)
  eps <- .08
  segments(x0 = lower, y0 = (n.studies:1) - eps, y1 = (n.studies:1) + eps)
  segments(x0 = upper, y0 = (n.studies:1) - eps, y1 = (n.studies:1) + eps)
  points(meta$data$y, n.studies:1,
    bg = "gray",
    pch = ifelse(lower < 0 & upper > 0, 21, 16)
  )

  if (!is.null(ss)) {
    segments(x0 = rand.low, x1 = rand.up, -3 * eps + n.studies:1, col = "gray")
    segments(
      x0 = rand.low, y0 = -2 * eps + (n.studies:1),
      y1 = -4 * eps + (n.studies:1), col = "darkgray"
    )
    segments(
      x0 = rand.up, y0 = -4 * eps + (n.studies:1),
      y1 = -2 * eps + (n.studies:1), col = "darkgray"
    )
    points(rand.est, -3 * eps + n.studies:1,
      col = "darkgray", pch = 24,
      cex = .6, bg = ifelse(rand.low < 0 & rand.up > 0,
        "white", "darkgray"
      )
    )
  }

  if (summary[2] == "hpd") {
    bnd <- grep("hpd", colnames(meta$estimates))
  } else {
    bnd <- grep("%", colnames(meta$estimates))[-2]
  }
  segments(
    y0 = -seq_len(n.ests),
    x0 = meta$estimates[sel, bnd[1]],
    x1 = meta$estimates[sel, bnd[2]], lwd = 2
  )
  segments(
    y0 = -seq_len(n.ests) - eps, y1 = -seq_len(n.ests) + eps,
    x0 = meta$estimates[sel, bnd[1]]
  )
  segments(
    y0 = -seq_len(n.ests) - eps, y1 = -seq_len(n.ests) + eps,
    x0 = meta$estimates[sel, bnd[2]]
  )
  points(meta$estimates[sel, summary[1]], -seq_len(n.ests), pch = 15, cex = 1.2)
}
