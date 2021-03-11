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
#' \item \code{"t"}: Student's t-distribution with \code{param = c(location, scale, nu)}
#'     where \code{nu} are the degrees of freedom (see \code{\link[LaplacesDemon]{dist.Student.t}}).
#' \item \code{"cauchy"}: Cauchy distribution with \code{param = c(location, scale)}.
#'     The Cauchy distribution is a special case of the t-distribution with degrees of freedom \code{nu=1}.
#' \item \code{"gamma"}: Gamma distribution with \code{param = c(shape, rate)}
#'     with rate parameter equal to the inverse scale (see \code{\link[stats]{GammaDist}}).
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
#' p1 <- prior("norm", c(mean = 0, sd = .3), lower = 0)
#' p1
#' p1(c(-1, 1, 3))
#' plot(p1, -.1, 1)
#'
#' ### Half-Cauchy Distribution
#' p2 <- prior("cauchy", c(location = 0, scale = .3), lower = 0)
#' plot(p2, -.5, 3)
#'
#' ### Custom Prior Distribution
#' p3 <- prior("custom", function(x) x^2, 0, 1)
#' plot(p3, -.1, 1.2)
#' @importFrom LaplacesDemon rinvgamma dst pst rst
#' @importFrom stats dbeta pbeta rbeta dgamma pgamma rgamma
#' @export
prior <- function(family, param, lower, upper, label = "d",
                  rel.tol = .Machine$double.eps^.5) {
  if (any(class(family) == "prior")) {
    attr(family, "label") <- label
    return(check_prior(family))
  }

  if (!is.character(family) || length(family) != 1) {
    stop("The prior must be defined by a character value. See ?metaBMA::prior")
  }

  family <- match.arg(family, c(
    "norm", "t", "beta", "invgamma", "gamma", "0", "custom",
    "scaledt", "cauchy", "halfcauchy", "halfnorm"
  ))

  # compatibility with old (non-Stan) metaBMA version
  if (family == "scaledt") {
    family <- "t"
  } else if (family == "cauchy") {
    stopifnot(length(param) == 2, param[2] > 0)
    family <- "t"
    param <- c(location = unname(param[1]), scale = unname(param[2]), nu = 1)
  } else if (family == "halfcauchy") {
    stopifnot(length(param) == 1, param > 0)
    family <- "t"
    param <- c(location = 0, scale = unname(param), nu = 1)
    lower <- 0
  } else if (family == "halfnorm") {
    stopifnot(length(param) == 1, param > 0)
    family <- "norm"
    param <- c(mean = 0, sd = unname(param))
    lower <- 0
  }

  # family-specific constraints on "param"
  if (family == "norm") {
    stopifnot(length(param) == 2, param[2] > 0)
  } else if (family == "t") {
    stopifnot(
      length(param) == 3, param[2] > 0, param[3] > 0,
      param[3] == round(param[3])
    )
  } else if (family == "invgamma" || family == "gamma") {
    stopifnot(length(param) == 2, all(param > 0))
    if (!missing(lower) && lower < 0) {
      warning('Lower truncation boundary for prior("',
              family, '", ...) is set to lower=0')
      lower <- 0
    }
  } else if (family == "beta") {
    stopifnot(length(param) == 2, all(param > 0))
  } else if (family == "0") {
    param <- vector("numeric", 0)
  }

  # custom probability density: needs normalization
  if (family == "custom") {
    stopifnot(is.function(param))
    if (missing(lower) || missing(upper) ||
      !is.numeric(lower) || !is.numeric(upper) ||
      is.na(lower) || is.na(upper) ||
      length(lower) != 1 || length(upper) != 1 ||
      lower > upper) {
      stop("'lower' and 'upper' must be specified (e.g., lower = 0, upper = Inf)")
    }
    dx <- param(seq(max(lower, -100), min(upper, 100), length.out = 11))
    if (length(dx) != 11 || any(is.na(dx) | dx < 0)) {
      stop("'param' must be a vectorized, nonnegative density function. See ?metaBMA::prior")
    }

    tryCatch(const <- integrate(param, lower, upper, rel.tol = rel.tol)$value,
      error = function(e) stop("Density must be integrable. See ?metaBMA::prior")
    )
    dprior <- function(x, log = FALSE) {
      dx <- ifelse(x >= lower & x <= upper, param(x) / const, 0)
      if (log) dx <- log(dx)
      dx
    }

    # available priors
  } else {
    if (missing(lower) && missing(upper)) {
      lower <- default_lower(family)
      upper <- default_upper(family)
      dprior <- switch(family,
        "norm" = function(x, log = FALSE) {
          dnorm(x, mean = param[1], sd = param[2], log = log)
        },
        "t" = function(x, log = FALSE) {
          dst(x, mu = param[1], sigma = param[2], nu = param[3], log = log)
        },
        "beta" = function(x, log = FALSE) {
          dbeta(x, shape1 = param[1], shape2 = param[2], log = log)
        },
        "invgamma" = function(x, log = FALSE) {
          dinvgamma(x, shape = param[1], scale = param[2], log = log)
        },
        "gamma" = function(x, log = FALSE) {
          dgamma(x, shape = param[1], rate = param[2], log = log)
        },
        "0" = function(x) {
          dx <- ifelse(x == 0, 1, 0)
          ifelse(log, log(dx), dx)
        },
        stop("Prior distribution 'family' not supported.")
      )
    } else {
      if (missing(lower)) lower <- default_lower(family)
      if (missing(upper)) upper <- default_upper(family)
      dprior <- switch(
        EXPR = family,
        "norm" = function(x, log = FALSE) {
          dtrunc(x, "norm", lower, upper, log = log,
                 mean = param[1], sd = param[2])
        },
        "t" = function(x, log = FALSE) {
          dtrunc(x, "st", lower, upper, log = log,
                 mu = param[1], sigma = param[2], nu = param[3])
        },
        "beta" = function(x, log = FALSE) {
          diff <- upper - lower
          dx <- dbeta((x - lower) / diff, param[1], param[2], log = log)
          ifelse(rep(log, length(x)), dx - log(diff), dx / diff)
        },
        "invgamma" = function(x, log = FALSE) {
          dtrunc(x, "invgamma", lower, upper, log = log,
                 shape = param[1], scale = param[2])
        },
        "gamma" = function(x, log = FALSE) {
          dtrunc(x, "gamma", lower, upper, log = log,
                 shape = param[1], rate = param[2])
        },
        "0" = function(x, log = FALSE) {
          dx <- ifelse(x == 0, 1, 0)
          ifelse(log, log(dx), dx)
        },
        stop("Prior distribution 'family' not supported.")
      )
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
priors <- function() {
  c("norm", "t", "beta", "invgamma", "gamma", "0", "custom")
}

# list of priors implemented in stan:
priors_stan <- function() {
  c("norm", "t", "beta", "invgamma", "gamma", "0")
} # for "0" => extra model file d=0

dtrunc <- function(x, family, lower = -Inf, upper = Inf, log = FALSE, ...) {
  stopifnot(lower < upper)
  support <- x >= lower & x <= upper
  dens <- rep(ifelse(log, -Inf, 0), length(x))
  g <- get(paste0("d", family), mode = "function")
  G <- get(paste0("p", family), mode = "function")
  if (log == TRUE) {
    dens[support] <- g(x[support], log = TRUE, ...) - log(G(upper, ...) - G(lower, ...))
  }
  else {
    dens[support] <- g(x[support], ...) / (G(upper, ...) - G(lower, ...))
  }
  dens
}

rtrunc <- function(n, family, lower = -Inf, upper = Inf, ...) {
  stopifnot(lower < upper)

  if (lower == -Inf && upper == Inf) {
    rng <- get(paste0("r", family), mode = "function")
    return(rng(n, ...))
  }

  cdf <- get(paste0("p", family), mode = "function")
  qdf <- get(paste0("q", family), mode = "function")

  u <- runif(n, cdf(lower, ...), cdf(upper, ...))
  qdf(u, ...)
}


#' @importFrom stats pgamma dgamma
dinvgamma <- function(x, shape = 1, scale = 1, log = FALSE) {
  ifelse(x == 0,
         0,
         LaplacesDemon::dinvgamma(x, shape = shape, scale = scale, log = log))
}

pinvgamma <- function(q, shape = 1, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  # p <- rep(0, length(q))
  # wikipedia:
  # pinvgamma = upper_inv_gamma(shape, scale/x) / gamma(shape)
  # and:
  # upper_inv_gamma(a, x) = pgamma(x, a, lower = FALSE) * gamma(a)
  pgamma(scale / q, shape, lower.tail = !lower.tail, log.p = log.p)
}

qinvgamma <- function(p, shape = 1, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  1 / qgamma(
    p = p, shape = shape, scale = 1 / scale,
    lower.tail = !lower.tail, log.p = log.p
  )
}
# u <- runif(100)
# u - pinvgamma(qinvgamma(u, shape = 3.5, scale = .34), shape = 3.5, scale = .34)


# ######## check
# b <- .12
# curve(pinvgamma(x, a, b), 0, 10, n = 1001)
# xx <- seq(0.001,10, .01)
# p <- sapply(xx, function(qq)
#   integrate(LaplacesDemon::dinvgamma, shape = a, scale = b, 0, qq)$value)
# lines(xx, p, col = 2, lty=2)
#
# curve(qinvgamma(x, a, b), 0, .95, n = 1001)
# lines(pinvgamma(xx, a, b), xx, col = "red", lty=2)
# abline(v=.95)
#
# xx1 <- rtrunc(50000, "invgamma", shape = a, scale = b, lower = 1)
# xx2 <- LaplacesDemon::rinvgamma(500000, shape = a, scale = b)
# qqplot(log(xx1), log(xx2[xx2 >= 1]))
# abline(0,1,col=2)
