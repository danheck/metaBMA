

#' @export
plot.prior <- function(x,
                       from = 0,
                       to = 1,
                       ...){
  prior <- x
  xx <- seq(from, to, length.out = 401)
  dpr <- prior(xx)

  yticks <- pretty(c(0, dpr))
  xticks <- pretty(c(from, to))
  Parameter <- 0
  plot(Parameter, -100, ylab = "Density", yaxs="i",
       ylim =  range(yticks), xlim = range(xticks), xaxs="i",
       las = 1, bty = "n",  ...)
  polygon(c(xx, rev(xx)), c(dpr, rep(0, 401)),
          border = NA, col=adjustcolor("darkgray", alpha.f = .2))
  lines(xx, dpr, col = "darkgray", lty = 1, lwd = 2)
}
