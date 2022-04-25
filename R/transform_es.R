#' Transformation of Effect Sizes
#'
#' Converts between different measures of effect size (i.e., Cohen's d, log odds
#' ratio, Pearson correlation r, and Fisher's z).
#'
#' @param y estimate of the effect size (can be vectorized).
#' @param SE optional: standard error of the effect-size estimate. Must have the
#'     same length as \code{y}.
#' @param from type of effect-size measure provided by the argument \code{y}.
#'     Supported effect sizes are
#'     Cohen's d (\code{"d"}),
#'     Fisher's z-transformed correlation (\code{"z"}),
#'     Pearson's correlation (\code{"r"}),
#'     or the log odds ratio (\code{"logOR"}).
#' @param to which type of effect size should be returned (see \code{from}).
#'
#' @details
#' The following chain of transformations is adopted from Borenstein et al. (2009):
#' \code{logOR <--> d <--> r <--> z}.
#' The conversion from \code{"d"} to \code{"r"} assumes equal sample sizes per condition (n1=n2).
#'
#' Note that in in a Bayesian meta-analysis, the prior distributions need to be adapted to the type of effect size. The function \code{\link{meta_default}} provides modified default prior distributions for different effect size measures which are approximately transformation-invariant (but results may still differ depending on which type of effect size is used for analysis).
#'
#' @return
#' If \code{SE} is missing, a vector of transformed effect sizes. Otherwise,
#' a matrix with two columns including effect sizes and standard errors.
#'
#' @template ref_borenstein2009
#'
#' @examples
#' # transform a single value of Cohen's
#' transform_es(y = 0.50, SE = 0.20, from = "d", to = "logOR")
#'
#' # towels data set:
#' transform_es(y = towels$logOR, SE = towels$SE, from = "logOR", to = "d")
#'
#' @seealso \code{\link{meta_default}}
#' @export
transform_es <- function(y, SE, from, to) {

  from <- match.arg(from, c("d", "logOR", "z", "r"))
  to <- match.arg(to, c("d", "logOR", "z", "r"))


  ########################### transform effect sizes
  stopifnot(is.numeric(y))

  if (from == "d") {
    if (to == "d") {
      x <- y

    } else if (to == "logOR") {
      x <- y * pi / sqrt(3)

    } else if (to == "r") {
      x <- y / sqrt(y^2 + 4)  # assumes n1=n2 for a=4=(n1+n2)^2/(n1*n2)

    } else if (to == "z") {
      r <- transform_es(y, from = "d", to = "r")
      x <- transform_es(r, from = "r", to = "z")
    }

  } else if (from == "logOR") {
    if (to == "d") {
      x <- y * sqrt(3) / pi

    } else if (to == "logOR") {
      x <- y

    } else if (to == "r") {
      d <- transform_es(y, from = "logOR", to = "d")
      x <- transform_es(d, from = "d", to = "r")

    } else if (to == "z") {
      d <- transform_es(y, from = "logOR", to = "d")
      r <- transform_es(d, from = "d", to = "r")
      x <- transform_es(r, from = "r", to = "z")
    }

  } else if (from == "r") {
    stopifnot(all(y >= -1), all(y <= 1))
    if (to == "d") {
      x <- 2*y / sqrt(1 - y^2)

    } else if (to == "logOR") {
      d <- transform_es(y, from = "r", to = "d")
      x <- transform_es(d, from = "d", to = "logOR")

    } else if (to == "r") {
      x <- y

    } else if (to == "z") {
      x <- atanh(y)  # 0.5 * (log(1+y) - log(1-y))

    }

  } else if (from == "z") {
    if (to == "d") {
      r <- transform_es(y, from = "z", to = "r")
      x <- transform_es(r, from = "r", to = "d")

    } else if (to == "logOR") {
      r <- transform_es(y, from = "z", to = "r")
      d <- transform_es(r, from = "r", to = "d")
      x <- transform_es(d, from = "d", to = "logOR")

    } else if (to == "r") {
      x <- tanh(y)  # (exp(2*y) - 1) / (exp(2*y) + 1)

    } else if (to == "z") {
      x <- y

    }
  }

  if (missing(SE))
    return(x)

  ########################### transform SE of effect sizes

  stopifnot(is.numeric(SE), all(SE > 0))
  stopifnot(length(y) == length(SE))

  if (from == "d") {
    if (to == "d") {
      SE_x <- SE

    } else if (to == "logOR") {
      SE_x <- SE * pi / sqrt(3)

    } else if (to == "r") {
      SE_x <- 4 * SE /(y^2 + 4)^(3/2) # assumes n1=n2 for a=4

    } else if (to == "z") {
      r <- transform_es(y, SE, from = "d", to = "r")
      SE_x <- transform_es(r[,1], r[,2], from = "r", to = "z")[,2]

    }

  } else if (from == "logOR") {
    if (to == "d") {
      SE_x <- SE * sqrt(3) / pi

    } else if (to == "logOR") {
      SE_x <- SE

    } else if (to == "r") {
      d <- transform_es(y, SE, from = "logOR", to = "d")
      SE_x <- transform_es(d[,1], d[,2], from = "d", to = "r")[,2]

    } else if (to == "z") {
      d <- transform_es(y, SE, from = "logOR", to = "d")
      r <- transform_es(d[,1], d[,2], from = "d", to = "r")
      SE_x <- transform_es(r[,1], r[,2], from = "r", to = "z")[,2]

    }

  } else if (from == "r") {
    if (to == "d") {
      SE_x <- 2*SE / (1 - y^2)^(3/2)

    } else if (to == "logOR") {
      d <- transform_es(y, SE, from = "r", to = "d")
      SE_x <- transform_es(d[,1], d[,2], from = "d", to = "logOR")[,2]

    } else if (to == "r") {
      SE_x <- SE

    } else if (to == "z") {
      SE_x <- sqrt(1 / ((1 - y^2)^2 / SE^2 - 2))
      # based on solving the following two equations:
      # (1) Var(z) = 1/(N-3)
      # (2) Var(r) = (1 - r^2)^2 / (N - 1)

    }

  } else if (from == "z") {
    if (to == "d") {
      r <- transform_es(y, SE, from = "z", to = "r")
      SE_x <- transform_es(r[,1], r[,2], from = "r", to = "d")[,2]

    } else if (to == "logOR") {
      r <- transform_es(y, SE, from = "z", to = "r")
      d <- transform_es(r[,1], r[,2], from = "r", to = "d")
      SE_x <- transform_es(d[,1], d[,2], from = "d", to = "logOR")[,2]

    } else if (to == "r") {
      SE_x <- (1 - x^2) / sqrt(1 / SE^2 + 2)  # x = correlation r (not Fisher's z!)

    } else if (to == "z") {
      SE_x <- SE

    }
  }

  X <- cbind(x, SE_x)
  colnames(X) <- c(to, "SE")
  X
}

