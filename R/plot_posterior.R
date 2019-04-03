#' Plot Posterior Distribution
#'
#' @param meta fitted meta-analysis model
#' @param parameter only for random-effects model: whether to plot \code{"d"} or \code{"tau"}
#' @inheritParams plot_forest
#' @param ... arguments passed to \code{\link[graphics]{plot}}
#'
#' @seealso \link{meta_bma}, \link{meta_fixed}, \link{meta_random}
#' @export
plot_posterior <- function (meta, parameter = "d", from, to,
                            summary = c("mean", "hpd"), ...) {
  UseMethod("plot_posterior", meta)
}


#' @export
plot_posterior.meta_fixed <- function (meta, parameter = "d", from, to,
                                       summary = c("mean", "hpd"), ...){
  NextMethod("plot_posterior", meta)
}

#' @export
plot_posterior.meta_random <- function (meta, parameter = "d", from, to,
                                        summary = c("mean", "hpd"), ...){
  NextMethod("plot_posterior", meta)
}


#' @export
plot_posterior.default <- function (meta, parameter = "d", from, to,
                                    summary = c("mean", "hpd"), ...){
  if (attr(meta$prior_d, "family") == "0" && parameter == "d")
    stop("H0: d = 0 can't be plotted." )

  dprior <- meta[[paste0("prior_",parameter)]]
  dpost<- meta[[paste0("posterior_",parameter)]]
  xlab <- ifelse(parameter == "tau",
                 "Heterogeneity of Effects",
                 ifelse(class(meta) == "meta_random",
                        "Random-Effects Mean", "Fixed-Effects Mean"))

  plot_density(dprior, dpost, stats = meta$estimates[parameter,],
               summary = summary, xlab = xlab, from = from, to = to, ...)
}


#' @export
plot_posterior.meta_bma <- function (meta, parameter = "d", from, to,
                                     summary = c("mean", "hpd"), ...){
  summary[1] <- match.arg(summary[1], c("mean", "50%"))
  summary[2] <- match.arg(summary[2], c("hpd", "bci"))
  if (parameter != "d")
    stop ("Plot for average effect only available for parameter='d'.\n",
          "  Run plot_posterior(fitted_meta_bma$meta$random_H1) to see heterogeneity.")

  prior_list <- meta[[paste0("prior_", parameter)]]
  dprior <- identical_prior(prior_list)

  others <- sapply(meta$meta, function (x) x$posterior_d)
  dpost.ave <- meta$posterior_d
  plot_density(dprior, dpost.ave, stats = meta$estimates["Averaged",],
               others = others, from = from, to = to, summary = summary,
               xlab = "Overall Effect", ...)
}


plot_density <- function (dprior, dpost, stats = NULL, others = NULL,
                          from, to, summary = c("mean", "hpd"), ...){

  # get x and y values
  if (missing (from)) from <- -Inf
  if (missing (to)) to <- Inf
  from <- max(from, attr(dprior, "lower"))
  to <- min(to, attr(dprior, "upper"))
  if (from == -Inf) from <- -1
  if (to == Inf) to <- 1

  xx <- seq(from, to, length.out = 501)
  dpr <- dprior(xx)
  dpo <- dpost(xx)
  if (!missing(others) && !is.null(others)){
    dx.others <- matrix(NA, 501, length(others))
    for(i in seq_along(others))
      dx.others[,i] <- others[[i]](xx)
  }else{
    dx.others <- NULL
  }

  # start drawing
  yticks <- pretty(c(0, dpo, dpr[dpr >= 0], c(dx.others)))
  xticks <- pretty(c(from, to))
  plot(NULL, NULL, ylab = "Density", yaxs="i",
       ylim =  range(yticks), xlim = range(xticks), xaxs="i",
       las = 1, bty = "n", ...)

  if (summary[2] == "hpd"){
    stats <- stats[c(summary[1], grep("hpd", names(stats), value = TRUE))]
  } else {
    stats <- stats[c(summary[1], grep("%", names(stats), value = TRUE)[-2])]
  }

  ########### PRIOR, POSTERIOR
  ltys <- c(5,1)
  cols <- c("darkgray","darkblue")
  if(!is.null(others)){
    ltys <- c(ltys, rep(2:4,20)[seq_along(others)])
    cols <- c(cols, colors()[31:60][seq_along(others)])
    for(i in seq_along(others))
      draw_dens(xx, dx = dx.others[,i], stats = NULL,
                lty = ltys[i+2], col = cols[i+2])
  }
  draw_dens(xx, dpr, stats = NULL, col = cols[1], lty = ltys[1])
  draw_dens(xx, dpo, stats = stats, col = cols[2], lty = ltys[2])

  post.label <- ifelse(is.null(dx.others),
                       "Posterior", "Posterior (averaged)")
  legend("topright", bty="n",
         legend = c("Prior",post.label, names(others)),
         col = cols, lty = ltys, lwd = 2)
}


draw_dens <- function (xx, dx, col, lty, stats = NULL){

  if (lty == 5 && all(dx >= 0)){
    polygon(c(xx, rev(xx)), c(dx, rep(0, 501)),
            border = NA, col=adjustcolor(col, alpha.f  = .2))
  } else if (lty == 1 & !is.null(stats)){
    sel <- xx > stats[2] & xx < stats[3]
    polygon(c(xx[sel], rev(xx[sel])), c(dx[sel], rep(0, 501)[sel]),
            border = NA, col=adjustcolor(col, alpha.f  = .2))
    sel <- which(abs(xx-stats[1])==min(abs(xx-stats[1])))
    segments(x0 = stats[1], x1 = stats[1], y0 = 0, y1 = dx[sel],
                 col = col, lwd = 2)
  }
  if(all(dx >= 0))
    lines(xx, dx, col = col, lty = lty, lwd = 2)
}
