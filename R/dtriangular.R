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
#' plot(prior("triangular", c(.2, .6, 1.3)), 0, 2)
#'
#' samples <- rtriangular(1e5, .2, .5, 1)
#' hist(samples, 200, FALSE)
#' curve(dtriangular(x, .2, .5, 1), col = 2,
#'       add = TRUE, lwd = 2)
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

#' @param n number of samples
#' @rdname dtriangular
#' @export
rtriangular <- function(n,
                        min,
                        peak,
                        max){
  u <- runif(n)
  Fc <- (peak - min) / (max - min)
  ifelse(u < Fc,
         min + sqrt(u * (max-min) * (peak - min)),
         max - sqrt((1 - u) * (max - min) * (max - peak)))
}
