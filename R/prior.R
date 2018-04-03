#' Prior Distributions
#'
#' Returns prior density function, e.g., for mean effect size \eqn{d} and heterogeneity of effect sizes \eqn{\tau}
#'
#' @param family a character value defining the distribution family
#' @param param numeric parameters for the distribution
#' @param label parameter label
#' @param lower only for \code{family = "custom"}: lower bound for prior density
#' @param upper only for \code{family = "custom"}: upper bound for prior density
#' @details
#' \itemize{
#' \item \code{"norm"}: Normal distribution with \code{param = c(mean, sd)} (see \code{\link[stats]{Normal}})
#' \item \code{"halfnorm"}: Half-normal distribution (positive) with \code{param = c(mean, sd)} (see \code{\link[LaplacesDemon]{dhalfnorm}})
#' \item \code{"truncnorm"}: Truncated normal distribution with \code{param = c(min, max, mean, sd)}  (see \code{\link{dtruncnorm}})
#' \item \code{"scaledt"}: Scaled Student-t with \code{param = c(mean, sigma, nu)} (see \code{\link[LaplacesDemon]{dist.Student.t}})
#' \item \code{"halft"}: Half-normal Student-t with \code{param = c(scale, df)} (see \code{\link[LaplacesDemon]{dist.Halft}})
#' \item \code{"cauchy"}: Cauchy distribution with \code{param = scale} (see \code{\link[stats]{Cauchy}})
#' \item \code{"halfcauchy"}: Half-Cauchy distribution (positive) with \code{param = scale}  (see \code{\link[LaplacesDemon]{dist.HalfCauchy}})
#' \item \code{"triangular"}: Triangular distribution with \code{param = c(min, peak, max)} (see \code{\link{dtriangular}})
#' \item \code{"beta"}: Beta distribution with \code{param = c(alpha, beta)} (see \code{\link[stats]{Beta}})
#' \item \code{"custom"}: User-specified prior density function defined by \code{param} (see examples; the density must be nonnegative and vectorized, but is normalized internally). Integration is performed from (-Inf, Inf), which requires that the function returns zeros (and not NAs) for values not in the support of the distribution.
#' }
#'
#' @return an object of the class \code{prior}: a density function with the arguments \code{x} (parameter values) and \code{log} (whether to return density or log-density)
#' @examples
#' ### Half-Normal Distribution
#' p1 <- prior("halfnorm", c(mean=0, sd=.3), "d")
#' p1
#' p1(c(-1,1,3))
#' plot(p1, -.1, 1)
#'
#' ### Half-Cauchy Distribution
#' p2 <- prior("halfcauchy", c(scale=.3))
#' plot(p2, -.5, 3)
#'
#' ### Custom Prior Distribution
#' p3 <- prior("custom", function(x) x^2, "d", 0, 1)
#' plot(p3, -.1, 1.2)
#' @export
prior <- function (family,
                   param,
                   label = "d",
                   lower = -Inf,
                   upper = Inf){

  if(any(class(family) == "prior")){
    attr(family, "label") <- label
    return(family)
  }

  if (!is.character(family) || length(family) != 1)
    stop ("The prior must be defined by a character value. See ?metaBMA::prior")

  npar <- length(param)
  if ( (family %in% c("truncnorm", "shiftedbeta") && npar != 4) ||
       (family %in% c("cauchy", "halfcauchy") && npar != 1) ||
       (family %in% c("norm", "halfnorm", "beta","halft") && npar != 2) ||
       (family %in% c("scaledt", "triangular") && npar != 3)
  ){
    stop ("Number of parameters does not match prior distribution. See ?metaBMA::prior")
  }

  if (family == "custom"){
    if (!is.function(param))
      stop ("'param' must be a (density) function. See ?metaBMA::prior")

    if(missing(lower) || missing(upper) ||
       !is.numeric(lower) || !is.numeric(upper) ||
       is.na(lower) || is.na(upper) ||
       length(lower) != 1 || length(upper) != 1 ||
       lower > upper)
      stop("'lower' and 'upper' must be specified (e.g., lower = 0, upper = Inf)")
    dx <- param(seq(max(lower, -100), min(upper, 100), length.out = 11))
    if (length(dx) != 11 || any(is.na(dx) | dx < 0))
      stop ("'param' must be a vectorized, nonnegative density function. See ?metaBMA::prior")

    tryCatch(const <- integrate(param, lower, upper,
                                rel.tol = .Machine$double.eps^0.4)$value,
             error = function(e) stop ("Density must be integrable. See ?metaBMA::prior"))

  } else if (family != "0" && !is.numeric(param)){
    stop ("'prior$param' must be numeric. See ?metaBMA::prior")
  }

  dprior <- switch(family,

                   "norm" = function (x, log = FALSE)
                     dnorm(x, param[1], param[2], log = log),
                   "halfnorm" = function (x, log = FALSE)  # mean, sd
                     dtruncnorm(x, 0, Inf, param[1], param[2], log = log),
                   "truncnorm" = function (x, log = FALSE)  # min, max, mean, sd
                     dtruncnorm(x, param[1], param[2], param[3], param[4], log = log),

                   "scaledt" = function (x, log = FALSE)  # mean, sigma, nu
                     dst(x, param[1],  param[2], param[3], log = log),
                   "halft" = function (x, log = FALSE)  #  sigma, nu
                     ifelse(x < 0, 0, dhalft(x, param[1], param[2], log = log)),

                   "cauchy" = function (x, log = FALSE)  # scale
                     dcauchy(x, 0, param[1], log = log),
                   "halfcauchy" = function (x, log = FALSE)  #  scale
                     ifelse(x < 0, 0, dhalfcauchy(x, param[1], log = log)),

                   "triangular" = function(x, log = FALSE)  # min, peak, max
                     dtriangular(x, param[1], param[2], param[3], log = log),
                   "beta" = function (x, log = FALSE)  # alpha, beta
                     dbeta(x, param[1], param[2], log = log),
                   "shiftedbeta" = function (x, log = FALSE)  # min, max, alpha, beta
                     dbeta((x-param[1])/param[2], param[3], param[4], log = log),

                   "0" = function (x, log = FALSE){
                     dx <- ifelse(x == 0, 1, 0)
                     ifelse(log, log(dx), dx)
                   },
                   "custom" = function (x, log = FALSE){
                     dx <- ifelse(x >= lower & x <= upper, param(x)/const, 0)
                     if(log) dx <- log(dx)
                     return(dx)
                   },

                   stop("Distribution not supported.")
  )

  class(dprior) <- "prior"
  bounds <- bounds_prior(family, param, label, lower, upper)
  attr(dprior, "family") <- family
  attr(dprior, "param") <- param
  attr(dprior, "label") <- label
  attr(dprior, "lower") <- bounds[1]
  attr(dprior, "upper") <- bounds[2]
  return (dprior)
}

