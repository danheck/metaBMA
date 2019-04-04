#' Prior Distribution
#'
#' Defines a prior distribution/probability density function for the
#' average effect size \eqn{d} or for the heterogeneity of effect sizes \eqn{\tau}.
#'
#' @param family a character value defining the distribution family.
#' @param param numeric parameters for the distribution. See details for the definition
#'     of the parameters of each family.
#' @param lower lower boundary for truncatation of prior density.
#'     If  \code{family="beta"}, the interval [0,1] is rescaled to the interval [lower,upper].
#'     Must be specified if \code{family = "custom"}.
#' @param upper See \code{lower}.
#' @param label optional: parameter label.
#' @param rel.tol relative tolerance used for integrating the density of \code{family="custom"}.
#'
#' @details
#' The following prior distributions are currently implemented:
#' \itemize{
#' \item \code{"norm"}: Normal distribution with \code{param = c(mean, sd)}
#'     (see \code{\link[stats]{Normal}}).
#' \item \code{"t"}: Student t distribution with \code{param = c(location, scale, nu)}
#'     (see \code{\link[LaplacesDemon]{dist.Student.t}}).
#'     Note that a Cauchy distribution is defined by setting the degrees of freedom \code{nu=1}.
#' \item \code{"invgamma"}: Inverse gamma distribution with \code{param = c(shape, scale)}
#'     (see \code{\link[LaplacesDemon]{dist.Inverse.Gamma}}).
#' \item \code{"beta"}: (Scaled) beta distribution with \code{param = c(shape1, shape2)}
#'     (see \code{\link[stats]{Beta}}).
#' \item \code{"custom"}: User-specified prior density function defined by \code{param}
#'     (see examples; the density must be nonnegative and vectorized, but is normalized
#'     internally). Integration is performed from (-Inf, Inf), which requires that the
#'     function returns zeros (and not NAs) for values not in the support of the distribution.
#' }
#'
#' @return an object of the class \code{prior}: a density function with the arguments
#'     \code{x} (parameter values) and \code{log} (whether to return density or log-density).
#'
#' @examples
#' ### Half-Normal Distribution
#' p1 <- prior("norm", c(mean=0, sd=.3), lower = 0)
#' p1
#' p1(c(-1,1,3))
#' plot(p1, -.1, 1)
#'
#' ### Half-Cauchy Distribution
#' p2 <- prior("t", c(location = 0, scale = .3, nu = 1), lower = 0)
#' plot(p2, -.5, 3)
#'
#' ### Custom Prior Distribution
#' p3 <- prior("custom", function(x) x^2, 0, 1)
#' plot(p3, -.1, 1.2)
#'
# ' Note: do not import LaplacesDemon::dinvgamma  => redefine: dinvgamma(0)=0
#' @importFrom LaplacesDemon rinvgamma dst pst rst
#' @importFrom stats dbeta pbeta rbeta
#' @export
prior <- function (family, param, lower, upper, label = "d",
                   rel.tol = .Machine$double.eps^.5){

  if (any(class(family) == "prior")){
    attr(family, "label") <- label
    return(check_prior(family))
  }

  if (!is.character(family) || length(family) != 1)
    stop ("The prior must be defined by a character value. See ?metaBMA::prior")

  family <- match.arg(family, c("norm", "t", "beta", "invgamma", "0", "custom",
                                "scaledt", "cauchy", "halfcauchy", "halfnorm"))

  # compatibility with old (non-Stan) metaBMA version
  switch(family,
         "scaledt" = {
           family = "t"},
         "cauchy" = {
           stopifnot(length(param) == 1, param > 0)
           family = "t"
           param = c(location = 0, scale = param, nu = 1)},
         "halfcauchy" = {
           stopifnot(length(param) == 1, param > 0)
           family = "t"
           param = c(location = 0, scale = param, nu = 1)
           lower = 0},
         "halfnorm" = {
           stopifnot(length(param) == 1, param > 0)
           family = "norm"
           param = c(mean = 0, sd = param)
           lower = 0}   )

  # family-specific constraints on "param"
  switch(family,
         "norm" = function(x, log = FALSE){
           stopifnot(length(param) == 2, param[2] > 0)},
         "t" = function(x, log = FALSE){
           stopifnot(length(param) == 3, param[2] > 0, param[3] > 0,
                     param[3] == round(param[3]))},
         "invgamma" = function(x, log = FALSE){
           stopifnot(length(param) == 2, all(param > 0))},
         "beta" = function(x, log = FALSE){
           stopifnot(length(param) == 2, all(param > 0))},
         "0" = { param <- vector("numeric", 0)}  )

  # custom probability density: needs normalization
  if (family == "custom"){
    stopifnot(is.function(param))
    if (missing(lower) || missing(upper) ||
        !is.numeric(lower) || !is.numeric(upper) ||
        is.na(lower) || is.na(upper) ||
        length(lower) != 1 || length(upper) != 1 ||
        lower > upper)
      stop("'lower' and 'upper' must be specified (e.g., lower = 0, upper = Inf)")
    dx <- param(seq(max(lower, -100), min(upper, 100), length.out = 11))
    if (length(dx) != 11 || any(is.na(dx) | dx < 0))
      stop ("'param' must be a vectorized, nonnegative density function. See ?metaBMA::prior")

    tryCatch(const <- integrate(param, lower, upper, rel.tol = rel.tol)$value,
             error = function(e) stop ("Density must be integrable. See ?metaBMA::prior"))
    dprior <- function (x, log = FALSE){
      dx <- ifelse(x >= lower & x <= upper, param(x)/const, 0)
      if(log) dx <- log(dx)
      dx
    }

    # available priors
  } else {
    if (missing(lower) && missing(upper)){
      lower <- default_lower(family)
      upper <- default_upper(family)
      dprior <- switch(family,
                       "norm" = function(x, log = FALSE){
                         dnorm(x, mean = param[1], sd = param[2], log = log)},
                       "t" = function(x, log = FALSE){
                         dst(x, mu = param[1], sigma = param[2], nu = param[3], log = log)},
                       "invgamma" = function(x, log = FALSE){
                         dinvgamma(x, shape = param[1], scale = param[2], log = log)},
                       "beta" = function(x, log = FALSE){
                         dbeta(x, shape1 = param[1], shape2 = param[2], log = log)},
                       "0" = function(x){
                         dx <- ifelse(x == 0, 1, 0)
                         ifelse(log, log(dx), dx)
                       },
                       stop("Prior distribution 'family' not supported."))
    } else {
      if (missing(lower)) lower <- default_lower(family)
      if (missing(upper)) upper <- default_upper(family)
      dprior <- switch(family,
                       "norm" = function (x, log = FALSE)
                         dtrunc(x, "norm", lower, upper, log=log,
                                mean = param[1], sd = param[2]),
                       "t" = function (x, log = FALSE)
                         dtrunc(x, "st", lower, upper, log=log,
                                mu = param[1], sigma = param[2], nu = param[3]),
                       "invgamma"  = function (x, log = FALSE)
                         dtrunc(x, "invgamma", lower, upper, log = log,
                                shape=param[1], scale=param[2]),
                       "beta" = function (x, log = FALSE){
                         diff <- upper - lower
                         dx <- dbeta((x - lower)/diff, param[1], param[2], log = log)
                         ifelse(rep(log, length(x)), dx - log(diff) , dx / diff)
                       },
                       "0" = function (x, log = FALSE){
                         dx <- ifelse(x == 0, 1, 0)
                         ifelse(log, log(dx), dx)
                       },
                       stop("Prior distribution 'family' not supported."))
    }
  }

  class(dprior) <- "prior"
  attr(dprior, "family") <- family
  attr(dprior, "param") <- param
  attr(dprior, "lower") <- lower
  attr(dprior, "upper") <- upper
  attr(dprior, "label") <- label
  check_prior(dprior, lower = attr(dprior, "lower"))
}

# list of available priors:
priors <- function()
  c("norm", "t", "beta", "invgamma", "0", "custom")

# list of priors implemented in stan:
priors_stan <- function()
  c("norm", "t", "beta", "invgamma", "0")   # for "0" => extra model file d=0

dtrunc <- function (x, family, lower = -Inf, upper = Inf, log = FALSE, ...){
  stopifnot(lower < upper)
  support <- x >= lower & x <= upper
  dens <- rep(ifelse(log, -Inf, 0), length(x))
  g <- get(paste0("d", family), mode = "function")
  G <- get(paste0("p", family), mode = "function")
  if (log == TRUE) {
    dens[support] <- g(x[support], log = TRUE, ...) - log(G(upper, ...) - G(lower, ...))
  }
  else {
    dens[support] <- g(x[support], ...)/(G(upper, ...) - G(lower, ...))
  }
  dens
}

#' @importFrom stats pgamma dgamma
pinvgamma <- function(q, shape = 1, scale = 1, lower.tail = TRUE, log.p = FALSE){
  # p <- rep(0, length(q))
  # wikipedia:
  # pinvgamma = upper_inv_gamma(shape, scale/x) / gamma(shape)
  # and:
  # upper_inv_gamma(a, x) = pgamma(x, a, lower = FALSE) * gamma(a)
  pgamma(scale/q, shape, lower.tail = !lower.tail, log.p = log.p)
}

dinvgamma <- function(x, shape=1, scale=1, log=FALSE)
  ifelse(x == 0, 0, LaplacesDemon::dinvgamma(x, shape, scale, log))

# check:
# a <- .3
# b <- 2
# curve(pinvgamma(x, a, b), 0, 10, n = 1001)
# xx <- seq(0.001,10, .01)
# # curve(LaplacesDemon::dinvgamma(x, shape = a, scale = b),0, 100, n= 1001)
# p <- sapply(xx, function(qq)
#   integrate(LaplacesDemon::dinvgamma, shape = a, scale = b, 0, qq)$value)
# lines(xx, p, col = 2, lty=2)
