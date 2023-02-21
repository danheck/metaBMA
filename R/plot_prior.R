#' Plot Prior Distribution
#'
#' Plot the probability density function of a prior distribution.
#'
#' @param x prior probability density function defined via \code{\link{prior}}.
#' @param from lower boundary
#' @param to upper boundary
#' @param ... further arguments passed to \code{\link[graphics]{plot}}
#'
#' @examples
#' p1 <- prior("t", c(location = 0, scale = 0.707, nu = 1), 0, 3)
#' plot(p1, 0, 2)
#'
#' # define custom prior pdf up to a constant:
#' p2 <- prior("custom", function(x) x^.5, 0, .5)
#' plot(p2)
#' @method plot prior
#' @export
plot.prior <- function(x, from, to, ...) {
  prior <- x

  if (missing(from))
    from <- attr(prior, "lower")
  if (missing(to))
    to <- attr(prior, "upper")
  # from <- max(from, attr(prior, "lower"))
  # to <- min(to, attr(prior, "upper"))
  if (is.infinite(from)) from <- - 1.5
  if (is.infinite(to)) to <- 1.5

  xx <- sort(c(0, seq(from, to, length.out = 401)))
  dpr <- prior(xx)

  yticks <- pretty(c(0, dpr))
  xticks <- pretty(c(from, to))
  Parameter <- 0
  Density <- -100
  plot(Parameter, Density,
    yaxs = "i", main = describe_prior(x),
    ylim = range(yticks), xlim = range(xticks), xaxs = "i",
    las = 1, bty = "n", ...
  )
  polygon(c(xx, rev(xx)), c(dpr, rep(0, length(xx))),
    border = NA, col = adjustcolor("darkgray", alpha.f = .2)
  )
  lines(xx, dpr, col = "darkgray", lty = 1, lwd = 2)
}
