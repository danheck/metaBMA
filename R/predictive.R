#' Predicted Bayes Factor for a New Study
#'
#' How much can be learned by an additional study? To judge this, this function samples the distribution of predicted Bayes factors for a new study given the current evidence.
#'
#' @param meta model-averaged meta-analysis (fitted with \code{\link{meta_bma}} or \code{\link{meta_default}})
#' @param SE expected standard error of future study. For instance, SE = 1/sqrt(N) for standardized effect sizes and N = sample size)
# ' @param n number of future studies (currently, only \code{n = 1} supported)
#' @param resample number of simulated Bayes factors
#' @export
predictive <- function(meta,
                       SE,
                       resample = 100){

  if (class(meta) != "meta_bma")
    stop ("Prediction only supported for models fitted via ?meta_bma or ?meta_default")

  # get information
  y <- meta$meta[[1]]$data$y
  SEs <- meta$meta[[1]]$data$SE
  labels <- meta$meta[[1]]$data$labels
  d <- attr(meta$meta[["Random Effects"]]$prior.d, "family")
  d.par <- attr(meta$meta[["Random Effects"]]$prior.d, "param")
  tau <- attr(meta$meta[["Random Effects"]]$prior.tau, "family")
  tau.par <- attr(meta$meta[["Random Effects"]]$prior.tau, "param")
  nstudies <- length(y)

  # sample parameters + study effect size(s)
  nsamples <- nrow(meta$meta[["Random Effects"]]$samples)
  nn <- round(resample * meta$posterior.models)
  replace <- ifelse(resample <= nsamples, FALSE, TRUE)

  #-------------- Fixed H0
  param.fH0 <- rep(0, nn["fixed.H0"])

  #-------------- Fixed H1
  ss.fixedH1 <- meta$meta[["Fixed Effects"]]$samples
  if (is.null(ss.fixedH1)){
    m.fixed.H1 <- meta_fixed(y, SE = SEs, d = d, d.par = d.par,
                             sample = 2 * nn["fixed.H1"],  summarize = "none")
    ss.fixedH1 <- m.fixed.H1$samples
  }
  param.fH1 <- sample(ss.fixedH1, nn["fixed.H1"], replace)

  #-------------- Random H0
  m.random.H0 <- meta_random(y, SEs, d = "0",  tau = tau, tau.par = tau.par,
                             sample = 2 * nn["random.H0"], summarize = "none")
  param.rH0 <- sample(m.random.H0$samples[,"tau"], nn["random.H0"], replace)

  #-------------- Random H1
  ss.randomH1 <- meta$meta[["Random Effects"]]$samples
  if (is.null(ss.randomH1)){
    m.random.H1 <- meta_random(y = y, SE = SEs, d = d, d.par = d.par, tau = tau,
                               tau.par = tau.par, sample = 2 * nn["random.H0"],
                               summarize = "none")
    ss.randomH1 <- m.random.H1$samples
  }
  idx <- sample(nrow(ss.randomH1), nn["random.H1"], replace)
  param.rH1 <-  ss.randomH1[idx, c("d.random", "tau")]

  #--------------  sample data sets
  d.pred <- c(rstudy(nn["fixed.H0"], "fixed", param.fH0, SE),
              rstudy(nn["fixed.H1"], "fixed", param.fH1, SE),
              rstudy(nn["random.H0"], "random", param.rH0, SE),
              rstudy(nn["random.H1"], "random", param.rH1, SE))

  #--------------  compute predicted Bayes factors
  meta_list <- list()
  for (i in seq_along(d.pred)){
    meta_list[[i]] <-
      meta_bma(c(y, d.pred[i]), c(SEs, SE), c(labels, "new"),
               d = d, d.par = d.par, tau = tau, tau.par = tau.par,
               sample = 0, prior = meta$prior.models, summarize = "none")

  }
  BF.predicted <- t(sapply(meta_list, function (x) unlist(x$BF)))
  BF.predicted <- cbind(d.pred = d.pred, BF.predicted)

  #--------------  compute observed Bayes factors
  BF.observed <- matrix(y[2:nstudies], nstudies - 1, ncol(BF.predicted),
                        dimnames = list(labels[2:nstudies], colnames(BF.predicted)))
  for(i in 1:nrow(BF.observed)){
    tmp <- meta_bma(y[1:i], SEs[1:i], labels[1:i],
                    d = d, d.par = d.par, tau = tau, tau.par = tau.par,
                    sample = 0, prior = meta$prior.models, summarize = "none")
    BF.observed[i,2:ncol(BF.observed)] <- unlist(tmp$BF)
  }

  meta_pred <- list("BF.observed" = BF.observed,
                    "BF.predicted" = BF.predicted)
  class(meta_pred) <- "meta_pred"
  return (meta_pred)
}
