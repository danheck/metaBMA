
#' Plot Posterior Distribution
#'
#' @param meta fitted meta-analysis model
#' @param parameter only for random-effects model: whether to plot \code{"d"} or \code{"tau"}
#' @param summary character vector with two values: either \code{"Mean"} or \code{"Median"}; and either highest-probability-density interval (\code{"HPD"}) or the quantile interval (\code{"quantile"})
#' @param from lower bound for x-axis
#' @param to upper bound for x-axis
#' @param ... arguments passed to \code{\link[graphics]{plot}}
#'
#' @seealso \link{meta_default}, \link{meta_bma}, \link{meta_fixed}, \link{meta_random}
#' @export
plot_posterior <- function(meta,
                           parameter = "d",
                           summary = c("Mean", "HPD"),
                           from = 0,
                           to = 1,
                           ...) {
  UseMethod("plot_posterior", meta)
}


# ' @rdname plot_posterior
#' @export
plot_posterior.meta_fixed <- function(meta,
                                      parameter = "d",
                                      summary = c("Mean", "HPD"),
                                      from = 0,
                                      to = 1,
                                      ...){
  NextMethod("plot_posterior", meta)
}

# ' @rdname plot_posterior
#' @export
plot_posterior.meta_random <- function(meta,
                                       parameter = "d",
                                       summary = c("Mean", "HPD"),
                                       from = 0,
                                       to = 1,
                                       ...){
  NextMethod("plot_posterior", meta)
}


# ' @rdname plot_posterior
#' @export
plot_posterior.default <- function(meta,
                                   parameter = "d",
                                   summary = c("Mean", "HPD"),
                                   from = 0,
                                   to = 1,
                                   ...){
  if(meta$data$prior.d$name == "0" && parameter == "d")
    stop("H0: d = 0 can't be plotted." )

  dprior <- prior(meta$data[[paste0("prior.",parameter)]])
  dpost<- posterior(meta, parameter = parameter)
  xlab <- ifelse(parameter == "tau",
                 "Heterogeneity of Effects",
                 ifelse(meta$data$model == "random",
                        "Random-Effects Mean", "Fixed-Effects Mean"))

  plot_density(dprior, dpost, stats = meta$estimates[parameter,],
               summary = summary, xlab = xlab, from = from, to = to, ...)
}


# ' @rdname plot_posterior
#' @export
plot_posterior.meta_bma <- function(meta,
                                    parameter = "d",
                                    summary = c("Mean", "HPD"),
                                    from = 0,
                                    to = 1,
                                    ...){
  if(parameter != "d")
    stop("Plot for average effect only available for parameter='d'.\n",
         "  Run plot_posterior(fitted_meta_bma$meta$random.H1) to see heterogeneity.")

  dprior <- prior(meta$meta$fixed.H1$data$prior.d)
  dpost.f <- posterior(meta$meta$fixed.H1, parameter = "d")
  dpost.r <- posterior(meta$meta$random.H1, parameter = "d")
  dpost.ave <- posterior(meta)

  plot_density(dprior, dpost.ave, stats = meta$estimates["averaged",], summary = summary,
               average = list(fixed = dpost.f, random = dpost.r),
               xlab = "Mean Effect", from = from, to = to, ...)
}


plot_density <- function(prior,
                         post,
                         stats = NULL,
                         summary = c("Mean", "HPD"),
                         average = NULL,
                         from = 0,
                         to = 1,
                         ...){
  xx <- seq(from, to, length.out = 501)
  dpr <- prior(xx)
  dpo <- post(xx)
  if(!missing(average) && !is.null(average)){
    dpo.fix <- average$fixed(xx)
    dpo.rand <- average$random(xx)
  }else{
    dpo.rand <- dpo.fix <- NULL
  }

  yticks <- pretty(c(0, dpo, dpr, dpo.rand, dpo.fix))
  xticks <- pretty(c(from, to))
  plot(NULL, NULL, ylab = "Density", yaxs="i",
       ylim =  range(yticks), xlim = range(xticks), xaxs="i",
       las = 1, bty = "n", ...)

  if(summary[2] == "HPD")
    stats <- stats[c(summary[1], "HPD95lower", "HPD95upper")]
  else
    stats <- stats[c(summary[1], "q025", "q975")]

  ########### PRIOR, POSTERIOR
  draw_dens(xx, dpr, "darkgray", "dashed")
  draw_dens(xx, dpo, "darkblue", "solid", stats = stats)

  if(!missing(average) && !is.null(average)){
    ########### POSTERIOR RANDOM, AVERAGED
    draw_dens(xx, dpo.rand, "darkred", "dotted")
    draw_dens(xx, dpo.fix, "darkgreen", "dotdash")

    legend("topright", legend = c("Prior","Posterior (averaged)",
                                  "Posterior (fixed)", "Posterior (random)"), bty="n",
           col = c("darkgray", "darkblue", "darkred", "darkgreen"),
           lty = c("dashed", "solid","dotted","dotdash"), lwd = 2)

  }else{
    legend("topright", legend = c("Prior","Posterior"), bty="n",
           col = c("darkgray", "darkblue"),
           lty = c("dashed", "solid"), lwd = 2)
  }
}


draw_dens <- function(xx, dx, col, lty, stats = NULL){
  if(missing(stats) | is.null(stats)){
    polygon(c(xx, rev(xx)), c(dx, rep(0, 501)),
            border = NA, col=adjustcolor(col, alpha.f  = .2))
  }else{
    sel <- xx > stats[2] & xx < stats[3]
    polygon(c(xx[sel], rev(xx[sel])), c(dx[sel], rep(0, 501)[sel]),
            border = NA, col=adjustcolor(col, alpha.f  = .2))
  }
  lines(xx, dx, col = col, lty = lty, lwd = 2)
  if(!is.null(stats)){
    sel <- which(abs(xx-stats[1])==min(abs(xx-stats[1])))
    segments(x0 = stats[1], y0 = 0, y1 = dx[sel], col = col, lwd = 2, lty = lty)
    # abline(v = stats[1], col = col, lwd = 2, lty = lty)
  }
}
