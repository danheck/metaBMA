

#' Forest Plot for Meta-Analysis
#'
#' Plots estimated effect sizes for all studies.
#'
#' @param meta fitted meta-analysis model
#' @param from extending the lower limit of the x-axis
#' @param to extending the upper limit of the x-axis
#' @param ... arguments passed to \code{\link[graphics]{plot}} (e.g., \code{from}, \code{to})
#'
#' @seealso \link{meta_default}, \link{meta_bma}, \link{meta_fixed}, \link{meta_random}
#' @export
plot_forest <- function(meta,
                        #ci = .95,
                        from = 0,
                        to = 1,
                        ...){
  UseMethod("plot_forest", meta)
}


# ' @rdname plot_posterior
#' @export
plot_forest.meta_fixed <- function(meta,
                                   #ci = .95,
                                   from = 0,
                                   to = 1,
                                   ...){
  plot_forest.default(meta,
                      main = "Fixed-Effects Meta-Analysis", ...)
  axis(2, -1, "Mean estimate", las = 1, tick = FALSE)
  par(mar = c(5.1,4.1, 4.1, 2.1))

}

# ' @rdname plot_posterior
#' @export
plot_forest.meta_random <- function(meta,
                                    # ci = .95,
                                    from = 0,
                                    to = 1,
                                    ...){
  plot_forest.default(meta,
                      main = "Random-Effects Meta-Analysis", ...)
  axis(2, -1, "Mean estimate", las = 1, tick = FALSE)
  par(mar = c(5.1,4.1, 4.1, 2.1))

}


# ' @rdname plot_posterior
#' @export
plot_forest.default <- function(meta,
                                # ci = .95,
                                from = 0,
                                to = 1,
                                ...){
  ci <- .95
  # if(meta$data$prior.d$name == "0" && parameter == "d")
  #   stop("H0: d = 0 can't be plotted." )

  n.studies <- length(meta$data$y)
  sel <- grep("d", rownames(meta$estimates))
  n.ests <- length(sel)
  lower <- meta$data$y - qnorm( (ci+1)/2) * sqrt(meta$data$V)  ################## REPLACE BY POSTERIOR ESTIMATE!!!
  upper <- meta$data$y + qnorm( (ci+1)/2) * sqrt(meta$data$V)
  xxx <- pretty(c(from, to, lower, upper))

  par(mar = c(4.5, 10, 4, .3))
  plot(NULL, xlim=range(xxx),
       yaxt = "n", ylab = "", las = 1,
       ylim = c( - n.ests, n.studies),
       bty = "n",  xlab = "Effect Size", ...)
  abline(v = 0, col = "gray")
  axis(2, n.studies:1, meta$data$labels, las = 1)

  points(meta$data$y, n.studies:1,
         pch = ifelse(lower < 0 & upper > 0, 21, 16))
  segments(x0 = lower,  x1 = upper,  n.studies:1)
  eps <- .08
  segments(x0 = lower,  y0 = (n.studies:1)-eps, y1 = (n.studies:1)+eps)
  segments(x0 = upper,  y0 = (n.studies:1)-eps, y1 = (n.studies:1)+eps)

  segments(y0 = -(1:n.ests),
           x0 = meta$estimates[sel,"q025"],
           x1 = meta$estimates[sel,"q975"], lwd = 2)
  segments(y0 = -(1:n.ests)-eps, y1 = -(1:n.ests)+eps,
           x0 = meta$estimates[sel,"q025"])
  segments(y0 = -(1:n.ests)-eps, y1 = -(1:n.ests)+eps,
           x0 = meta$estimates[sel,"q975"])
  points(meta$estimates[sel,"Mean"], -(1:n.ests), pch = 15, cex = 1.2)
}


# ' @rdname plot_posterior
#' @export
plot_forest.meta_bma <- function(meta,
                                 # ci = .95,
                                 from = 0,
                                 to = 1,
                                 ...){
  meta$data <- meta$meta$random.H1$data
  plot_forest.default(meta,
                      main = "Meta-Analysis with Model-Averaging", ...)
  axis(2, -(1:3), c("Averaged", "Fixed effects", "Random effects"),
       las = 1, tick = TRUE)
  par(mar = c(5.1,4.1, 4.1, 2.1))

  # if(parameter != "d")
  #   stop("Plot for average effect only available for parameter='d'.\n",
  #        "  Run plot_posterior(fitted_meta_bma$meta$random.H1) to see heterogeneity.")
  #
  # dprior <- prior(meta$meta$fixed.H1$data$prior.d)
  # dpost.f <- posterior(meta$meta$fixed.H1, parameter = "d")
  # dpost.r <- posterior(meta$meta$random.H1, parameter = "d")
  # dpost.ave <- posterior(meta)
  #
  # plot_density(dprior, dpost.ave, stats = meta$estimates["averaged",],
  #              average = list(fixed = dpost.f, random = dpost.r),
  # xlab = "Mean Effect", from = from, to = to, ...)
}
