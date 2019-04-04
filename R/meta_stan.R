# ' Fit a Bayesian Meta-Analysis Using Stan
# '
# ' Uses Stan to draw posterior samples of a fixed-effects or random-effects meta-analysis
# ' with or without moderator variables. To compute Bayes factor and marginal probabilities,
# ' use \code{\link{meta_fixed}}, \code{\link{meta_random}}, and \code{\link{meta_bma}}.
# '
# ' @return a fitted stan model (see \code{\link[rstan]{stanfit-class}}).
# '     Note that the regression parameters are not meaningfully labeled.
#' @import rstan
#' @importFrom utils capture.output
meta_stan <- function (data_list,
                       d = prior("norm", c(mean = 0, sd = .3), lower = 0),
                       tau  = prior("t", c(location = 0, scale = .5, nu = 1), lower = 0),
                       jzs = list(rscale_contin = 1/2,
                                  rscale_discrete = sqrt(2)/2,
                                  centering = TRUE),
                       ml_init = TRUE,
                       silent_stan = TRUE,
                       ...){

  data_list <- c(data_list, prior_as_list(d))
  if (grepl("random", data_list$model)){
    attr(tau, "label") <- "tau"
    data_list <- c(data_list, prior_as_list(tau))
  }
  if (attr(d, "family") == "0")
    data_list$model <- paste0(data_list$model, "_H0")  # not possible: JZS + H0

  data_list <- add_jzs(data_list, jzs)
  if (grepl("jzs", data_list$model))
    stop("Moderators are not supported if truncated=TRUE")

  # default settings for stan
  dots <- list(...)
  if (is.null(dots$iter)) dots$iter <- 3000
  if (is.null(dots$warmup)) dots$warmup <- min(dots$iter/2, 500)
  if (is.null(dots$control)) dots$control <- list("adapt_delta" = .95,
                                                  "max_treedepth" = 20)
  if (ml_init && is.null(dots$init))
    dots$init = function() ml_estimates(data_list$y, data_list$SE,
                                        model = data_list$model,
                                        d = d, tau = tau,
                                        normal_noise = .05)

  args <- c(list("object" = stanmodels[[data_list$model]],
                 "data" = data_list), dots)
  args$data$data <- args$data$model <- args$data$labels <-
    args$data$model.frame <- NULL

  if (silent_stan)
    capture.output(fit <- do.call("sampling", args))
  else
    fit <- do.call("sampling", args)

  fit
}


prior_as_list <- function (prior){
  par <- attr(prior, "label")
  prior <- check_prior(prior, lower = ifelse(par == "tau", 0, -Inf))
  param <- attr(prior, "param")

  family_idx <- match(attr(prior, "family"), priors_stan())
  data_list <- list("family" = as.integer(family_idx),
                    "param" = param,
                    "bnd" = bounds_prior(prior))
  if (attr(prior, "family") == "0")
    data_list$bnd[2] <- 1
  if (length(param) < 3)
    data_list$param <- c(param, rep(-1, 3 - length(param)))
  names(data_list) <- paste0(par, "_", names(data_list))
  data_list
}


# translate formula into data objects
add_jzs <- function (data_list, jzs){
  mf <- data_list$model.frame
  if (is.null(mf))
    return (data_list)

  # find out which moderators are continuous / discrete / disallow interactions
  formula <- attr(mf, "terms")
  terms <- attr(formula, "term.labels")
  if(any(grepl(":", terms, fixed = TRUE)))
    stop("Interaction terms are currently not supported.")
  discr_l <- !sapply(mf[terms], is.numeric)
  with_contin <- any(!discr_l)
  contin <- names(discr_l)[!discr_l]
  discr <- names(discr_l)[discr_l]
  number_levels <- unlist(sapply(mf[discr], function(x) length(unique(x))))

  B <- as.integer(any(!discr_l) + sum(discr_l))           # number of JZS blocks (1 contin + B-1 discrete)
  if (B == 0)
    return(data_list)
  P <- array(c(sum(!discr_l)[with_contin], number_levels - 1), dim = B)   # number of slope parameters per block
  rscale <- array(c(jzs$rscale_contin[with_contin],
                    rep(jzs$rscale_discrete, B - with_contin)), dim = B)
  L <- array(0, c(B, max(P), max(P)))            # cholesky of solve(var(X)) per block
  b_idx <- cbind(from = cumsum(c(1, P))[seq_along(P)],
                 to = cumsum(P))
  rownames(b_idx) <- names(P) <- names(rscale) <- c("contin"[with_contin], discr)

  X <- matrix(NA, nrow(mf), sum(P),
              dimnames = list(data_list$labels, seq(sum(P))))

  # continuous moderators: all captured within the first JZS block
  if (with_contin){
    idx_c <- 1:P[1]
    X[,idx_c] <- do.call("cbind", mf[contin])
    if (jzs$centering)
      X[,idx_c] <- scale(X[,idx_c,drop = FALSE], scale = FALSE)
    colnames(X)[idx_c] <- contin
    L[1, idx_c, idx_c] <- chol_inv_cov(X[,idx_c])
  }

  # discrete moderators: modeled in separate JZS blocks
  #       => centering for discrete variables?! (unbalanced designs?!)
  if (any(discr_l)){
    for (i in seq_along(discr)){
      idx_d <- sum(P[seq_len(i - 1 + with_contin)]) + seq_len(P[i + with_contin])
      X[,idx_d] <- Xd <- design_matrix(mf, discr[i])
      colnames(X)[idx_d] <- colnames(Xd)
      L[i + with_contin, seq(P[i]), seq(P[i])] <- diag(ncol(Xd))
    }
  }

  c(data_list, list(B = B, P = P, X = X, L = L, b_idx = b_idx, rscale = rscale))
}


# construct design matrix with fixed-effects contrasts (Rouder & Morey, 2012)
#
# mf: a model.frame/data.frame/list
# discr: the name of the discrete factor variable
design_matrix <- function(model.frame, discr){

  model.frame[[discr]] <- as.factor(model.frame[[discr]])
  levels <- sort(unique(levels(model.frame[[discr]])))
  number_levels <- length(levels)
  stopifnot(number_levels > 1)

  Z <- diag(number_levels) - 1/number_levels
  ev <- eigen(Z, symmetric = TRUE)$vectors
  contrasts <- as.matrix(ev[,seq(number_levels - 1)])
  rownames(contrasts) <- levels

  contrasts(model.frame[[discr]]) <- contrasts
  X <- model.matrix(as.formula(paste("~", discr)), model.frame)
  # drop intercept:
  X[,attr(X, "assign") != 0, drop = FALSE]
}


# cholesky decomposition (stan needs lower triangular): V = L %*% t(L)
chol_inv_cov <- function(X){
  if (is.null(dim(X))) X <- matrix(X)
  V <- solve(var(X) * (nrow(X) - 1) / nrow(X))
  t(chol(V))  # = L
}

