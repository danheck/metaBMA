#' Bayesian Meta-Analysis Using Stan
#'
#' Uses Stan to draw posterior samples of a fixed-effects or random-effects meta-analysis
#' with or without moderator variables.
#'
#' @inheritParams meta_bma
#' @param model whether to fit a \code{"random"} or \code{"fixed"} effects meta-analysis
#'     (can be abbreviated by \code{"r"} and \code{"f"}).
#' @param d parameters defining the prior on the mean effect size d
#'     (parameters of a truncated t distribution: df, location, scale, lower, upper).
#' @param tau parameters defining the prior on the heterogeneity tau
#'     (parameters of a truncated t distribution: df, location, scale, lower, upper).
#' @param scale scale parameter(s) of the JZS prior for the moderators.
#'     If a scalar is provided, the same prior is used for all JZS blocks.
#'     If a vector is provided, the scale values are assined in the same order as
#'     defined in the formula \code{y}.
#' @param show whether to print Stan output.
#' @param ... further arguments that are passed to \code{rstan::sampling}
#'     (see \code{\link[rstan]{stanmodel-method-sampling}}), for instance:
#'     \code{iter=5000}, \code{warmup=500}, \code{chains=4},
#'     \code{control=list(adapt_delta=.95)}
#'
#' @return a fitted stan model (see \code{\link[rstan]{stanfit-class}})
#'
#' @import rstan
#' @importFrom utils capture.output
#'
#' @examples
#' data(towels)
#'
#' # random-effects:
#' meta_stan(logOR, SE, study, towels, model = "r")
#'
#' # fixed-effects with formula interface:
#' meta_stan(logOR ~ 1, SE, study, towels, model = "f")
#' @export
meta_stan <- function (y, SE, labels, data, model = c("random", "fixed"),
                       d = c(1, 0, .3, -Inf, Inf),
                       tau = c(1, 0, .5, 0, Inf),
                       scale = .5, show = FALSE, ...){
  dl <- data_list_eval(model, y, SE, labels, data, as.list(match.call()))

  check_prior(d)
  check_prior(tau, lower = 0)
  dl$df_d <- as.integer(d[1])
  dl$p_d <- d[2:5]
  if (dl$model == "random"){
    dl$df_tau <- as.integer(tau[1])
    dl$p_tau <- tau[2:5]
  }

  dl <- add_jzs(dl, scale)

  if (show){
    stanfit <- sampling(stanmodels[[dl$model]], data = dl, ...)
  } else {
    capture.output(stanfit <- sampling(stanmodels[[dl$model]], data = dl, ...))
  }
  stanfit
}

# translate formula into data objects
add_jzs <- function (data_list, scale){
  if (is.null(data_list$model.frame))
    return (data_list)

  ######################### TODO : JZS BLOCKS / DISCRETE-CONTINUOUS

  # find out which moderators are continuous / discrete / disallow interactions
  formula <- attr(data_list$model.frame, "terms")
  X <- model.matrix(formula, data_list$model.frame)
  # X must be centered (for continuous covariates. for discrete?)
  X <- scale(X[,attr(X, "assign") != 0,drop = FALSE], scale = FALSE)
  attr(X, "scale") <- NULL


  P <- array(ncol(X), dim = 1)
  if (all(P == 0))
    return(data_list)
  scale <- array(scale, dim = 1)
  L <- array(1, c(1,P,P))
  L[1,,] <- chol_inv_cov(X)
  b_idx <- cbind(1, P)

  data_list$model <- paste0(data_list$model, "_JZS")
  c(data_list,
    list(B = 1L, P = P, X = X, L = L, b_idx = b_idx, s = scale))
}


# cholesky decomposition (stan needs lower triangular): V = L %*% t(L)
chol_inv_cov <- function(X){
  V <- solve(var(X))
  L <- t(chol(V))
  L
}

