#' metaBMA: Bayesian Model Averaging for Random and Fixed Effects Meta-Analysis
#'
#' @author Daniel W. Heck & Quentin F. Gronau
#' @docType package
#' @useDynLib metaBMA
#'
#' @importFrom runjags run.jags runjags.options combine.mcmc
#' @importFrom Matrix nearPD
#' @importFrom mvtnorm rmvnorm dmvnorm
#' @import stats
#' @importFrom Brobdingnag as.brob
#' @importFrom coda spectrum0.ar HPDinterval as.mcmc varnames mcmc
#' @importFrom LaplacesDemon dhalfcauchy dhalft dst
#' @importFrom graphics plot legend lines
#' @references
#' Heck, D. W. & Gronau, Q. F. (2017). metaBMA: Bayesian Model Averaging for Random and Fixed Effects Meta-Analysis.
#'
#'
"_PACKAGE"


