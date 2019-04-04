#' Meta-Analysis with Order-Constrained Study Effects
#'
#' Computes the Bayes factor for the hypothesis that the true study effects in a
#' random-effects meta-anaysis are all positive or negative.
#'
#' @inheritParams meta_bma
#' @param prior prior probabilities over models (possibly unnormalized) in the
#'     order \code{c(fixed_H0, fixed_H1, ordered_H1, random_H1)}. Note that the
#'     model \code{random_H0} is not included in the comparison.
#'
#' @details
#' The Bayes factor for the order-constrained model is computed using the
#' encompassing Bayes factor.
#'
#' @examples
#' data(towels)
#' debug(meta_ordered)
#' mo <- meta_ordered(logOR, SE, study, towels,
#'                    d = prior("norm", c(mean=0, sd=.3), lower=0),
#'                    tau = prior("invgamma", c(shape = 1, scale = 0.15)))
#' mo
#' plot_posterior(mo, "d")
#' @seealso \link{meta_random}
#' @template ref_haaf2019
#' @export
meta_ordered <- function (y, SE, labels, data,
                          d = prior("norm", c(mean = 0, sd = .3), lower = 0),
                          tau  = prior("invgamma", c(shape = 1, scale = 0.15)),
                          prior = c(1,1,1,1),
                          logml = "integrate", summarize = "stan", ci = .95,
                          rel.tol = .Machine$double.eps^.3, ...){

  # check whether constraints are one-directional (+/- 0)
  if (attr(d, "lower") == 0 && attr(d, "upper") == Inf)
    direction <- "positive"
  else if (attr(d, "lower") == 0 && attr(d, "upper") == Inf)
    direction <- "negative"
  else
    stop("The study effects in the random-effects meta-analysis must\n",
         "  be constrained to be either positive or negative, e.g.:\n",
         "    d=prior('norm', c(mean=0, sd=.3), lower=0, upper=Inf) \n",
         "    d=prior('norm', c(mean=0, sd=.3), lower=-Inf, upper=0) ")

  # start with standard meta_bma
  args <- as.list(match.call())
  if (is.null(args$iter)) args$iter <- 20000
  meta_ordered <- do.call("meta_bma", args)
  samples <- extract(meta_ordered$meta$random$stanfit_dstudy, "dstudy")[["dstudy"]]
  check_post <- apply(samples >= attr(d, "lower") &
                      samples <= attr(d, "upper"), 1, all)
  cnt_post <- list("cnt" = sum(check_post),
                   "M" = length(check_post))

  # prior sampling
  mcmc_prior <- list("d" = rprior(args$iter, d),
                     "tau" = rprior(args$iter, tau))
  N <- length(meta_ordered$meta$fixed$data$y)
  p_pos <- exp(N * pnorm(0, mcmc_prior$d, mcmc_prior$tau,
                         lower.tail = direction == "negative", log.p = TRUE))
  p_prior <- mean(p_pos)
  p_prior
  bf_ordered1 <- (cnt_post$cnt / cnt_post$M) / p_prior

  # check_prior <- rep(NA, args$iter)
  # for (i in 1:args$iter)
  #   check_prior[i] <- all(rnorm(N, mcmc_prior$d[i], mcmc_prior$tau[i]) > 0)
  # mean(check_prior)


  # get estimates for order constraints
  if (cnt_post$cnt >= 2000){
    meta_ordered$meta[[direction]] <- meta_ordered$meta$random
    dtau <- do.call("cbind", extract(meta_ordered$meta$random$stanfit_dstudy, c("d", "tau")))
    meta_ordered$meta[[direction]]$estimates <- summary_ordered <-
      t(apply(dtau[check_post,], 2, summary_samples))

    meta_ordered$meta[[direction]]$stanfit_dstudy <- NULL
    meta_ordered$meta[[direction]]$model <- "random_truncated"
    meta_ordered$meta[[direction]]$posterior_d <-
      posterior_logspline(dtau[,"d"], "d", d)
    meta_ordered$meta[[direction]]$posterior_tau <-
      posterior_logspline(dtau[,"tau"], "tau", tau)

  } else {
    args_trunc <- args
    args_trunc$truncation <- TRUE
    meta_ordered$meta[[direction]] <- do.call("meta_random", args_trunc)
  }
  # bf_ordered0 <- bf_ordered1 * meta_ordered$BF$
  meta_ordered$meta[[direction]]$BF <- c("ordered_vs_random" = bf_ordered1,
                                         "ordered_vs_null" = NA)
  # bf_ordered0 = exp(logml_ordered - logml_0)
  # meta_ordered$meta[[direction]]$logml <- log(bf_ordered0) + meta$
  meta_ordered$meta[[direction]]$logml  <- NA


  # postproces internal structure of meta-bma
  modelname <- paste0(direction)
  names(meta_ordered$logml) <-
    names(meta_ordered$prior_models) <-
    names(meta_ordered$posterior_models) <-
    c("null", "fixed", modelname, "random")
  meta_ordered$estimates <- rbind(meta_ordered$estimates["fixed",,drop=FALSE],
                                  summary_ordered[direction,,drop=FALSE],
                                  meta_ordered$estimates["random",,drop=FALSE])

  meta_ordered$posterior_d <- function(x) rep(-1, length(x))

  meta_ordered$BF <- list("fixed_vs_H0" = meta_ordered$BF$d_10_fixed,
                          "ordered_vs_random" = bf_ordered)


  # TODO
  meta_ordered$posterior_models
  meta_ordered$logml
  # PRINT function

  meta_ordered
}

################  prior sampling not possible with stan files
#                 (requires generated quantities!)
# args_prior <- c(list("data_list" = meta_ordered$meta$random$data,
#                      "d" = d, "tau" = tau, "ml_init" = FALSE),
#                 list(...))
# args_prior$iter <- args$iter
# args_prior$data_list$N <- 0
# args_prior$data_list$y <- args_prior$data_list$SE <- vector("numeric")
# fit_prior <- do.call("meta_stan", args_prior)
# cnt_prior <- count_dstudy(fit_prior, prior = d)


# count how many prior/posterior samples are inside constraints
count_dstudy <- function(stanfit, prior){
  samples <- extract(stanfit, "dstudy")[["dstudy"]]
  check_post <- apply(samples >= attr(prior, "lower") &
                      samples <= attr(prior, "upper"), 1, all)
  list("cnt" = sum(check_post),
       "M" = length(check_post))
}
