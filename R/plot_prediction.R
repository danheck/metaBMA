#' Plot Predicted Bayes Factors
#'
#' @param x an object of the class \code{"prediction"} which contains observed
#'     and predicted Bayes factors
#' @param which a character value defining which Bayes factor to plot
#'     (one of "d_10_fixed", "d_10_random", "d_10_averaged",
#'     "H1_fixed_vs_random")
#' @param scale either plots Bayes factors (\code{"BF"}), inverse Bayes factors
#'     (\code{"1/BF"}), log Bayes factors (\code{"log"}), or the log-inverse
#'     Bayes factor (\code{"1/log"})
#' @param ... arguments passed to \code{\link[graphics]{plot}}
#' @method plot meta_pred
#' @export
plot.meta_pred <- function(x,
                           which = "d_10_averaged",
                           scale = "BF",
                           ...) {
  nstudies <- nrow(x$BF.observed) + 1
  BFo <- x$BF.observed[, which]
  BFp <- unlist(x$BF.predicted[, which])
  switch(scale,
    "log" = {
      BFo <- log(BFo)
      BFp <- log(BFp)
    },
    "1/BF" = {
      BFo <- 1 / BFo
      BFp <- 1 / BFp
    },
    "1/log" = {
      BFo <- 1 / log(BFo)
      BFp <- 1 / log(BFp)
    }
  )


  bf.dens <- density(BFp)
  bf.hist <- hist(BFp, plot = FALSE)
  # bf.spline <- logspline(BFp, lbound = ifelse(scale %in% c("BF", "1/BF"), 0, -Inf))
  # dd.y <- seq(bf.spline$range[1], bf.spline$range[2], length = 301)
  # dd.x <- dlogspline(dd.y, bf.spline)
  # dd.x <- dd.x / max(dd.x)
  bf.dens$y <- bf.dens$y / max(bf.dens$y)
  bf.hist$density <- bf.hist$density / max(bf.hist$density)
  xx <- pretty(c(2, nstudies + 1 + 1))
  yy <- pretty(c(0, BFo, BFp, ifelse(scale == "log", 0, 1)))
  Study <- -1
  BF <- -1

  plot(Study, BF,
    ylim = range(yy), xlim = range(xx),
    main = "Predicted Bayes Factors for New Study",
    type = "l", bty = "n"
  )
  abline(
    h = ifelse(scale == "log", 0, 1), lwd = 2,
    col = "darkgray", lty = "dashed"
  )
  lines(2:nstudies, BFo, lwd = 2)
  for (i in seq_along(BFp)) {
    lines(nstudies + 0:1, c(BFo[nstudies - 1], BFp[i]),
      col = adjustcolor("darkgray", alpha.f = .2), lwd = 2
    )
  }


  barplot(c(0, bf.hist$density), diff(c(0, bf.hist$breaks)),
    axes = FALSE,
    space = 0, horiz = TRUE, add = TRUE, border = "darkgray",
    offset = nstudies + 1
  )

  lines(bf.dens$y + nstudies + 1, bf.dens$x, lwd = 2, col = "darkblue")
  # lines(dd.x + nstudies + 1, dd.y, lwd = 2, col = "darkblue")
  points(nstudies + 1, mean(BFp),
    pch = 16, cex = 1.3,
    col = "darkblue"
  )
  segments(
    x0 = nstudies + 1, col = "darkblue", lwd = 2,
    y0 = mean(BFp) - sd(BFp),
    y1 = mean(BFp) + sd(BFp)
  )
  eps <- .1
  segments(
    x0 = nstudies + 1 - eps, col = "darkblue",
    x1 = nstudies + 1 + eps,
    y0 = mean(BFp) - sd(BFp)
  )
  segments(
    x0 = nstudies + 1 - eps, col = "darkblue",
    x1 = nstudies + 1 + eps,
    y0 = mean(BFp) + sd(BFp)
  )
  points(2:nstudies, BFo, pch = 16)


  ## 3) determine barplot and height parameter
  ## histogram (for barplot-ting the density)
  # , breaks=seq(from=min(x[,1]), to=max(x[,1]),
  #                                  #           length.out=lhist))
  # ## determine the plot range and all the things needed for the barplots and lines
  # yx <- seq(min(x[,2]), max(x[,2]), length.out = num.dnorm)
  # yy <- dnorm(yx, mean=mean(x[,2]), sd=sd(x[,2]))

  ## barplot and line for y (right)
  # par(mar=c(0, 0, 0, 0))
}
