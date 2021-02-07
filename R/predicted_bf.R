#' Predicted Bayes Factors for a New Study
#'
#' How much can be learned by an additional study? To judge this, this function
#' samples the distribution of predicted Bayes factors for a new study given the
#' current evidence.
#'
#' @param meta model-averaged meta-analysis (fitted with \code{\link{meta_bma}}).
#' @param SE a scalar: the expected standard error of future study.
#'     For instance, SE = 1/sqrt(N) for standardized effect sizes and N = sample size)
#' @param sample number of simulated Bayes factors
#' @param ... further arguments passed to rstan::sampling to draw posterior samples for d and tau.
#' @export
predicted_bf <- function(meta, SE, sample = 100, ...) {
  if (class(meta) != "meta_bma") {
    stop("Prediction only supported for models fitted via ?meta_bma")
  }

  check_data_identical(meta$meta)
  y <- meta$meta[[1]]$data$y
  SEs <- meta$meta[[1]]$data$SE
  labels <- meta$meta[[1]]$data$labels
  d <- meta$meta$random$prior_d
  tau <- meta$meta$random$prior_tau
  nstudies <- length(y)

  # sample parameters + study effect size(s)
  nn <- round(sample * meta$posterior_models)
  param.fH1 <- param.rH0 <- param.rH1 <- c()

  #-------------- Fixed H0
  param.fH0 <- rep(0, nn["fixed_H0"])

  #-------------- Fixed H1
  if (nn["fixed_H1"] > 0) {
    ss.fixedH1 <- meta$meta$fixed$stanfit
    if (is.null(ss.fixedH1)) {
      ss.fixedH1 <- meta_stan(
        data_list = meta$meta[[1]]$data,
        d = meta$meta[["fixed"]]$prior_d, ...
      )
    }
    param.fH1 <- sample(extract(ss.fixedH1, "d", FALSE), nn["fixed_H1"])
  }

  #-------------- Random H0
  data_list <- meta$meta[["random"]]$data
  if (nn["random_H0"] > 0) {
    ss.randomH0 <- meta_stan(data_list,
      d = prior("0"),
      tau = meta$meta[["random"]]$prior_tau, ...
    )
    param.rH0 <- sample(extract(ss.randomH0, "tau", FALSE), nn["random_H0"])
  }

  #-------------- Random H1
  if (nn["random_H1"] > 0) {
    ss.randomH1 <- meta$meta$random$stanfit
    if (is.null(ss.randomH1)) {
      ss.randomH1 <- meta_stan(data_list,
        d = meta$meta[["random_H1"]]$prior_d,
        tau = meta$meta[["random_H1"]]$prior_tau, ...
      )
    }
    mcmc <- do.call("cbind", extract(ss.randomH1, c("d", "tau")))
    idx <- sample(nrow(mcmc), nn["random_H1"])
    param.rH1 <- mcmc[idx, c("d", "tau")]
  }

  #--------------  sample data sets
  d.pred <- c(
    rstudy(nn["fixed_H0"], "fixed", param.fH0, SE),
    rstudy(nn["fixed_H1"], "fixed", param.fH1, SE),
    rstudy(nn["random_H0"], "random", param.rH0, SE),
    rstudy(nn["random_H1"], "random", param.rH1, SE)
  )

  #--------------  compute predicted Bayes factors
  meta_list <- list()
  for (i in seq_along(d.pred)) {
    meta_list[[i]] <-
      meta_bma(c(y, d.pred[i]), c(SEs, SE), c(labels, "new"),
        d = d, tau = tau,
        prior = meta$prior_models, ...
      )
  }
  BF.predicted <- t(sapply(meta_list, function(x) unlist(x$BF)))
  BF.predicted <- cbind(d.pred = d.pred, BF.predicted)

  #--------------  compute observed Bayes factors
  BF.observed <- matrix(y[2:nstudies], nstudies - 1, ncol(BF.predicted),
    dimnames = list(labels[2:nstudies], colnames(BF.predicted))
  )
  for (i in 2:nstudies) {
    tmp <- meta_bma(y[1:i], SEs[1:i], labels[1:i],
      d = d, tau = tau,
      prior = meta$prior_models, ...
    )
    BF.observed[i - 1, 2:ncol(BF.observed)] <- unlist(tmp$BF)
  }

  meta_pred <- list(
    "BF.observed" = BF.observed,
    "BF.predicted" = BF.predicted
  )
  class(meta_pred) <- "meta_pred"
  meta_pred
}
