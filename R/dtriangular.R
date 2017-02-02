#' Triangular Distribution
#'
#' Distribution with a triangular density function.
#'
#' @param x vector of quantiles
#' @param min lower bound of distribution
#' @param peak peak of triangle density
#' @param max upper bound of distribution
#' @param log logical; if TRUE, probabilities p are given as log(p)
#'
#' @examples
#' curve(dtriangular(x, min = .2, peak = .6, max = 1.3),
#'       from = 0, to = 2, n = 301)
#'
#' plot(prior("triangular", c(.2, .6, 1.3)), 0, 2)
#' @export
dtriangular <- function(x,
                        min,
                        peak,
                        max,
                        log = FALSE){

  if (any(c(length(min), length(max), length(peak)) != 1))
      stop("'dtriangular' does not allow vectorized arguments min, peak, max.")
  if (! (min < peak) && (peak < max))
    stop("Parmameters must be ordered: min < peak < max.")


  const <- (max - min) / 2
  s1 <- 1 / (const * (peak - min))
  i1 <- - s1 * min
  s2 <- - 1 / (const * (max - peak))
  i2 <- - s2 * max

  fx <- function(x) ifelse(x < min | x > max, 0,
                           ifelse(x < peak ,
                                  i1 + s1 * x,
                                  i2 + s2 * x))
  dx <- fx(x)
  if (log)
    dx <- log(dx)
  return (dx)
}

