describe_prior <- function(prior, digits = 3) {
  if (is.null(prior) || class(prior) != "prior") {
    return(NULL)
  }

  family <- attr(prior, "family")
  param <- attr(prior, "param")
  if (is.numeric(param)) {
    param <- round(param, digits)
  }
  labels <- switch(family,
    "norm" = paste0("mean=", param[1], ", sd=", param[2]),
    "t" = paste0("location=", param[1], ", scale=", param[2], ", nu=", param[3]),
    "invgamma" =  paste0("shape=", param[1], ", scale=", param[2]),
    "beta" =  paste0("shape1=", param[1], ", shape2=", param[2]),
    "0" = paste0("H0: ", attr(prior, "label"), " = 0) "),
    "custom" = paste0("user-specified function"),
    stop("Distribution not supported.")
  )

  if (family == "custom") {
    truncated <- TRUE
  } else {
    truncated <-
      default_lower(family) != attr(prior, "lower") ||
        default_upper(family) != attr(prior, "upper")
  }
  trunc <- switch(family,
    "beta" = ") rescaled to the interval",
    "0" = "",
    ifelse(truncated,
      ") truncated to the interval",
      ") with support on the interval"
    )
  )
  paste0(
    "'", attr(prior, "family"), "' (", labels, trunc, " [",
    attr(prior, "lower"), ",", attr(prior, "upper"), "]."
  )
}


#' @export
print.prior <- function(x, ...) {
  cat("Prior density function (class='prior'):", describe_prior(x), "\n")
}


#' @importFrom LaplacesDemon rinvgamma rst dst pst qst
rprior <- function(n, prior) {
  if (is.null(prior)) {
    return(NULL)
  }

  param <- attr(prior, "param")
  lower <- attr(prior, "lower")
  upper <- attr(prior, "upper")

  truncated <- lower != default_lower(attr(prior, "family")) ||
    upper != default_upper(attr(prior, "family"))

  if (truncated) {
    x <- switch(attr(prior, "family"),
      "norm" = LaplacesDemon::rtrunc(n,
        spec = "norm", lower, upper,
        mean = param[1], sd = param[2]
      ),
      "t" = LaplacesDemon::rtrunc(n, "st", lower, upper,
        mu = param[1], sigma = param[2], nu = param[3]
      ),
      # custom function for inverse gamma:
      "invgamma" = rtrunc(n, "invgamma", lower, upper,
        shape = param[1], scale = param[2]
      ),
      "beta" = lower + (upper - lower) * rbeta(n, param[1], param[2]),
      "0" = rep(0, n),
      "custom" = stop("Custom prior not supported."),
      stop("Distribution not supported.")
    )
  } else {
    x <- switch(attr(prior, "family"),
      "norm" = rnorm(n, mean = param[1], sd = param[2]),
      "t" = rst(n, mu = param[1], sigma = param[2], nu = param[3]),
      "invgamma" = LaplacesDemon::rinvgamma(n, shape = param[1], scale = param[2]),
      "beta" = rbeta(n, param[1], param[2]),
      "0" = rep(0, n),
      "custom" = stop("Custom prior not supported."),
      stop("Distribution not supported.")
    )
  }
  x
}


truncnorm_mean <- function(mean, sd, lower, upper) {
  alpha <- (lower - mean) / sd
  beta <- (upper - mean) / sd
  diff_cdf <- pnorm(beta) - pnorm(alpha)
  diff_pdf <- dnorm(alpha) - dnorm(beta)
  mean + sd * diff_pdf / diff_cdf
}

log_diff_exp <- function(logx1, logx2) {
  c <- logx1
  log(exp(logx1 - c) - exp(logx2 - c)) + c
}

############## random sampling for custom prior not supported
# lb <- attr(prior, "lower")
# ub <- attr(prior, "upper")
# px <- function(p) sapply(p, function(pp)
#   integrate(prior, lb, pp)$value )
# u <- runif(n, lb, ub)
#
# xx <- seq(max(lb, 0), min(ub, 3), length.out = 201)
# cnt <- 1
# while (px(xx[1]) > .002){
#   cnt <- cnt + 1
#   newx <- seq(min(xx)-1, min(xx), length.out = 41)
#   xx <- c(newx, xx)
# }
# cnt <- 1
# while (px(max(xx)) < .998){
#   cnt <- cnt + 1
#   newx <- seq(from = max(xx), to = max(xx)+1, length.out = 41)
#   xx <- c(xx, newx)
# }
# dx <- prior(xx)
# px <- cumsum(dx)
# qdens <- splinefun(px, xx)
# length.interval <- function (start){
#   qdens(start+ci)-qdens(start)
# }
# oo <- optim((1-ci)/2, length.interval, method = "L-BFGS-B",
#             lower=min(px), upper=max(px)-ci)
# if (log) dx <- log(dx)
# return (dx)
