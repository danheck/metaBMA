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
#' Usually, in random-effects meta-analysis,the study-specific random-effects
#' are allowed to be both negative or positive even when the prior on the
#' overall effect size \code{d} is truncated to be positive). In contrast, the
#' function \code{meta_ordered} tests a model in which the random effects are
#' forced to be either all positive or all negative. The direction of the
#' study-specific random-effects is defined via the prior on \code{d}. For
#' instance, \code{d=prior("norm", c(0,.5), lower=0)} means that all
#' random-effects are positive (not just the overall mean effect size).
#'
#' The Bayes factor for the order-constrained model is computed using the
#' encompassing Bayes factor. Since many posterior samples are required for this
#' approach, the default number of MCMC iterations for \code{meta_ordered} is
#' \code{iter=10000} per chain. The posterior summary statistics for the model
#' \code{ordered} refer to the overall effect size (i.e., the expected value of
#' the truncated normal distribution) and not to the location parameter \code{d}
#' (which is not the expected value of a truncated normal).
#'
#' @examples
#' ### Bayesian Meta-Analysis with Order Constraints
#' # Note: The following code optimizes speed (for CRAN checks).
#' #       The settings are not suitable for actual data analysis!
#'
#' data(towels)
#' set.seed(123)
#' mo <- meta_ordered(logOR, SE, study, towels,
#'                    d = prior("norm", c(mean=0, sd=.3), lower=0),
#'                    rel.tol=.Machine$double.eps^.15, iter=2000)
#' mo
#' plot_posterior(mo)
#' @seealso \link{meta_bma}, \link{meta_random}
#' @template ref_haaf2019
#' @export
meta_ordered <- function (y, SE, labels, data,
                          d = prior("norm", c(mean = 0, sd = .3), lower = 0),
                          tau  = prior("invgamma", c(shape = 1, scale = 0.15)),
                          prior = c(1,1,1,1),
                          logml = "integrate", summarize = "stan", ci = .95,
                          rel.tol = .Machine$double.eps^.3,
                          silent_stan = TRUE, ...){

  check_deprecated(list(...))  # error: backwards compatibility
  if (attr(d, "lower") == -Inf && attr(d, "upper") == Inf)
    stop("The study-specific effects in the ordered random-effects meta-analysis must\n",
         "be order-constrained/truncated, e.g., to be all positive or all negative:\n",
         "    d=prior('norm', c(mean=0, sd=.3), lower=0, upper=Inf) \n",
         "    d=prior('norm', c(mean=0, sd=.3), lower=-Inf, upper=0) ")

  # start with standard meta_bma
  args <- as.list(match.call())[-1]
  if (is.null(args$iter)) args$iter <- 10000
  m_ordered <- do.call("meta_bma", args)
  names(m_ordered$logml) <-
    names(m_ordered$prior_models) <-
    names(m_ordered$posterior_models) <-
    c("null", "fixed", "ordered", "random")

  # count posterior samples
  samples <- extract(m_ordered$meta$random$stanfit_dstudy, "dstudy")[["dstudy"]]
  check_post <- apply(samples >= attr(d, "lower") &
                      samples <= attr(d, "upper"), 1, all)
  cnt_post <- list("cnt" = sum(check_post),
                   "M" = length(check_post))
  p_post <- cnt_post$cnt / cnt_post$M

  # count prior samples
  # (note: prior sampling not possible with current stan files)
  mcmc_prior <- list("d" = rprior(args$iter, d),
                     "tau" = rprior(args$iter, tau))
  N <- length(m_ordered$meta$fixed$data$y)
  if (attr(d, "upper") == Inf)   # positive REs
    logp_dstudy <- pnorm(attr(d, "lower"), mcmc_prior$d, mcmc_prior$tau,
                         lower.tail = FALSE, log.p = TRUE)
  else if (attr(d, "lower") == -Inf)  # negative REs
    logp_dstudy <- pnorm(attr(d, "upper"), mcmc_prior$d, mcmc_prior$tau,
                         lower.tail = TRUE, log.p = TRUE)
  else
    logp_dstudy <- log_diff_exp(
      pnorm(attr(d, "upper"), mcmc_prior$d, mcmc_prior$tau, log.p = TRUE),
      pnorm(attr(d, "lower"), mcmc_prior$d, mcmc_prior$tau, log.p = TRUE))

  p_pos <- exp(N * logp_dstudy)
  p_prior <- mean(p_pos)
  bf_ordered1 <- p_post / p_prior

  # check_prior <- rep(NA, args$iter)
  # for (i in 1:args$iter)
  #   check_prior[i] <- all(rnorm(N, mcmc_prior$d[i], mcmc_prior$tau[i]) > 0)
  # mean(check_prior)


  # get parameter estimates for order-constrained model
  m_ordered$meta$ordered <- m_ordered$meta$random
  m_ordered$meta$ordered$model <- m_ordered$meta$ordered$data$model <-
    "random_ordered"

  if (cnt_post$cnt >= 2000){
    m_ordered$meta$ordered$stanfit_dstudy <- NULL
    samples <- do.call("cbind", extract(m_ordered$meta$random$stanfit_dstudy,
                                        c("d", "tau")))[check_post,]
  } else{
    m_ordered$meta$ordered$stanfit_dstudy <-
      meta_stan(m_ordered$meta$ordered$data, d, tau, silent_stan = silent_stan, ...)
    samples <- do.call("cbind", extract(m_ordered$meta$ordered$stanfit_dstudy,
                                        c("d", "tau")))
  }
  # # check truncated sampling  vs. rejection sampling:
  # qqplot(samples2[,"d"], samples[,"d"]) ; abline(0,1,col=2)
  # qqplot(samples2[,"tau"], samples[,"tau"]) ; abline(0,1,col=2)
  # ks.test(samples2[,"d"], samples[,"d"])
  average_effect <- truncnorm_mean(samples[,"d"], samples[,"tau"],
                                   attr(d, "lower"), attr(d, "upper"))
  samples <- cbind(samples, average_effect)
  m_ordered$meta$ordered$estimates <- t(apply(samples, 2, summary_samples))
  m_ordered$meta$ordered$posterior_d <-
    posterior_logspline(samples[,"d"], "d", d)
  m_ordered$meta$ordered$posterior_tau <-
    posterior_logspline(samples[,"tau"], "tau", tau)

  # p(y|Mo) = B_{Mo,Mr} * p(y|Mr)
  m_ordered$logml[["ordered"]] <- log(bf_ordered1) + m_ordered$meta$random$logml["random_H1"]
  m_ordered$meta$ordered$logml  <- c("fixed_H0" = unname(m_ordered$meta$fixed$logml["fixed_H0"]),
                                     "ordered_H1" = m_ordered$logml[["ordered"]])
  m_ordered$meta$ordered$BF <- make_BF(m_ordered$meta$ordered$logml)

  # postproces structure of meta-bma object
  m_ordered$estimates <-
    rbind(m_ordered$estimates["fixed",,drop=FALSE],
          m_ordered$meta$ordered$estimates["average_effect",,drop=FALSE],
          m_ordered$estimates["random",,drop=FALSE])
  rownames(m_ordered$estimates)[2] <- "ordered"

  m_ordered$posterior_d <- m_ordered$meta$ordered$posterior_d

  m_ordered$BF <- make_BF(m_ordered$logml)
  m_ordered$inclusion <- inclusion(m_ordered$logml, include = c(2:4), prior = prior)
  m_ordered$prior_models <- m_ordered$inclusion$prior
  m_ordered$posterior_models <- m_ordered$inclusion$posterior

  m_ordered
}


# count how many prior/posterior samples are inside constraints
count_dstudy <- function(stanfit, prior){
  samples <- extract(stanfit, "dstudy")[["dstudy"]]
  check_post <- apply(samples >= attr(prior, "lower") &
                      samples <= attr(prior, "upper"), 1, all)
  list("cnt" = sum(check_post),
       "M" = length(check_post))
}