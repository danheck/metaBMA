#' Prior Distributions
#'
#' Supported prior distributions for mean effect size \eqn{d} and heterogeneity of effect sizes \eqn{\tau}
#'
#' @param prior a named list with the two arguments \code{name} (character) and \code{par} (numeric vector)
#' @param log whether to return log density
#' @details
#' \itemize{
#' \item \code{"norm"}: Normal distribution with \code{par = c(mean, sd)} (see \code{\link[stats]{dnorm}})
#' \item \code{"halfnorm"}: Half-normal distribution (positive) with \code{par = c(mean, sd)} (see \code{\link[LaplacesDemon]{dhalfnorm}})
#' \item \code{"truncnorm"}: Truncated normal distribution with \code{par = c(min, max, mean, sd)}  (see \code{\link{dtruncnorm}})
#' \item \code{"scaledt"}: Scaled Student-t with \code{par = c(mean, sigma, nu)} (see \code{\link[LaplacesDemon]{dst}})
#' \item \code{"halft"}: Half-normal Student-t with \code{par = c(scale, df)} (see \code{\link[LaplacesDemon]{dhalft}})
#' \item \code{"cauchy"}: Cauchy distribution with \code{par = scale} (see \code{\link[stats]{dcauchy}})
#' \item \code{"halfcauchy"}: Half-Cauchy distribution (positive) with \code{par = scale}  (see \code{\link[LaplacesDemon]{dhalfcauchy}})
#' \item \code{"beta"}: Beta distribution with \code{par = c(alpha, beta)} (see \code{\link[stats]{dbeta}})
# \item \code{"shiftedbeta"}: Shifted beta distribution with \code{par = c(min, max, alpha, beta)}
#' }
#' @return a log-density function with a single argument for the values to be evaluated (used for integration internally)
#' @examples
#' ### Half-Normal Distribution
#' p1 <- log_prior(list(name = "halfnorm",
#'                 par = c(0, .3)))
#' p1(c(-1,1,3))
#' curve(exp(p1(x)),
#'       from = -.5, to = 1.5, n = 501)
#'
#' ### Half-Cauchy Distribution
#' p2 <- log_prior(list(name = "halfcauchy",
#'                 par = .3))
#' curve(exp(p2(x)),
#'       add=TRUE, from=0, col=2, n = 501)
#' @export
log_prior <- function(prior, log = TRUE){

  name <- prior$name
  par <- prior$par

  if( (name %in% c("truncnorm", "shiftedbeta") && length(par) != 4) ||
      (name %in% c("cauchy", "halfcauchy") && length(par) != 1) ||
      (name %in% c("norm", "halfnorm", "beta","halft") && length(par) != 2) ||
      (name %in% c("scaledt") && length(par) != 3)
  ){
    stop("Number of parameters does not match prior distribution.")
  }

  switch(name,
         "norm" =
           function(x) dnorm(x, par[1], par[2], log = log),
         "halfnorm" =  # mean, sd
           function(x) dtruncnorm(x, 0, Inf, par[1], par[2], log = log),
         "truncnorm" =  # min, max, mean, sd
           function(x) dtruncnorm(x, par[1], par[2], par[3], par[4], log = log),
         "scaledt" =  # mean, sigma, nu
           function(x) dst(x, par[1],  par[2], par[3], log = log),
         "halft" =  #  sigma, nu
           function(x) dhalft(x, par[1], par[2], log = log),
         "cauchy" = # scale
           function(x) dcauchy(x, 0, par[1], log = log),
         "halfcauchy" =  #  scale
           function(x) dhalfcauchy(x, par[1], log = log),
         "beta" = # alpha, beta
           function(x) dbeta(x, par[1], par[2], log = log),
         "shiftedbeta" = # min, max, alpha, beta
           function(x) dbeta((x-par[1])/par[2], par[3], par[4], log = log),
         "0" =
           function(x) ifelse(x == 0, 1, 0),
         stop("Distribution not supported.")
  )
}

