#' Truncated Normal Distribution
#'
#' Normal distribution truncated to the interval [a,b]
#'
#' @param a lower bound
#' @param b upper bound
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @param log if TRUE, probabilities are given as log(p)
#' @examples
#' curve(dtruncnorm(x,-.5,3.5, 1,1), -2,4, n = 501)
#' @export
dtruncnorm <- function(x,
                       a = -Inf,
                       b = Inf,
                       mean = 0,
                       sd = 1,
                       log = FALSE){

  if(a == -Inf && b == Inf){
    dx <- dnorm(x, mean, sd, log=log)

  }else{
    p.lower <- pnorm(a, mean, sd, lower.tail = TRUE)
    p.upper <- pnorm(b, mean, sd, lower.tail = FALSE)

    dx <- sel <- (x >= a) & (x <= b)
    if(any(sel)){
      dx[sel] <- dnorm(x[sel], mean, sd, log=log)
      if(log){
        dx[!sel] <- -Inf
        dx[sel] <- dx[sel] - log(1 - p.lower - p.upper)
      }else{
        dx[sel] <- dx[sel] / (1 - p.lower - p.upper)
      }
    }
  }
  return(dx)
}
# x <- rnorm(1000)
# all.equal(truncnorm::dtruncnorm(c(x), a= 0, b=Inf, mean = 0, sd = .3),
# dtruncnorm(c(x), a= 0, b=Inf, mean = 0, sd = .3))


