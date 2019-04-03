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
#' # data(towels)
#' # mo <- meta_ordered(logOR, SE, study, towels,
#' #                    d = prior("norm", c(mean=0, sd=.3), lower=0),
#' #                    tau = prior("t", c(location=0, scale=.5, nu=1), lower=0))
#' # mo
#' # plot_posterior(mo, "d")
#' @seealso \link{meta_random}
#' @template ref_haaf2019
#' @export
meta_ordered <- function (y, SE, labels, data,
                          d = prior("norm", c(mean = 0, sd = .3), lower = 0),
                          tau  = prior("t", c(location = 0, scale = .5, nu = 1), lower = 0),
                          # rscale_contin = 1/2, rscale_discrete = sqrt(2)/2, centering = TRUE,
                          prior = c(1,1,1,1),
                          logml = "integrate", summarize = "stan", ci = .95,
                          rel.tol = .Machine$double.eps^.3, ...){

}
