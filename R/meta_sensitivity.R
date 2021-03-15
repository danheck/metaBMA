#' Sensitivity Analysis for Bayesian Meta-Analysis
#'
#' Sensitivity analysis assuming different prior distributions for the two main
#' parameters of a Bayesian meta-analysis (i.e., the overall effect and the
#' heterogeneity of effect sizes across studies).
#'
#' @inheritParams meta_bma
#' @param d_list a `list` of prior distributions specified via [prior()]
#'   for the overall effect size (mean) across studies
#' @param tau_list a `list` of prior distributions specified via [prior()]
#'   for the heterogeneity (SD) of effect sizes across studies
#' @param analysis which type of meta-analysis should be performed for analysis?
#'   Can be one of the following:
#'   * `"fixed"` for fixed-effects model, see [meta_fixed()]
#'   * `"random"` for random-effects model, see [meta_random()]
#'   * `"bma"` for model averaging, see [meta_bma()]
#' @param combine_priors either `"matched"`, in which case the analysis
#'   includes the *matched* pairwise combinations of the prior distributions
#'   specified in `d_list` and `tau_list`, or `crossed`, in
#'   which case the analysis uses *all possible* pairwise combinations of priors.
#' @param ... further arguments passed to the function specified in `analysis`.
#'
#' @return
#' an object of the S3 class `meta_sensitivity`, that is, a list of fitted
#' meta-analysis models. Results can be printed or plotted using
#' [plot.meta_sensitivity()].
#'
#' @examples
#' \donttest{
#' data(towels)
#' sensitivity <- meta_sensitivity(
#'   y = logOR, SE = SE, labels = study, data = towels,
#'   d_list = list(prior("cauchy", c(0, .707)),
#'                 prior("norm", c(0, .5)),
#'                 prior("norm", c(.5, .3))),
#'   tau_list = list(prior("invgamma", c(1, 0.15), label = "tau"),
#'                   prior("gamma", c(1.5, 3), label = "tau")),
#'   analysis = "random",
#'   combine_priors = "crossed")
#'
#' print(sensitivity, digits = 2)
#'
#' par(mfrow = c(1,2))
#' plot(sensitivity, "d", "prior")
#' plot(sensitivity, "d", "posterior")
#'
#' plot(sensitivity, "tau", "prior")
#' plot(sensitivity, "tau", "posterior")
#' }
#'
#' @seealso [plot.meta_sensitivity()]
#' @export
meta_sensitivity <- function(y, SE, labels, data,
                             d_list,
                             tau_list,
                             analysis = "bma",
                             combine_priors = "crossed",
                             ...){

  analysis <- match.arg(analysis, c("fixed", "random", "bma"))

  # check priors:
  stopifnot(class(d_list) == "list", length(d_list) >= 1)
  if(!all(sapply(d_list, class) == "prior"))
    stop("The argument d_list must provide a list of prior distributions",
         "\nspecified via the function ?metaBMA::prior")

  d_list2 <- d_list

  # prior on "tau" only relevant for random/bma analysis
  if (analysis != "fixed"){

    stopifnot(class(tau_list) == "list",  length(tau_list) >= 1)
    if(!all(sapply(tau_list, class) == "prior"))
      stop("The argument tau_list must provide a list of prior distributions",
           "\nspecified via the function ?metaBMA::prior")

    # expand crossed combinations of priors into matched lists
    combine_priors <- match.arg(combine_priors, c("matched", "crossed"))
    if (combine_priors == "matched")
      stopifnot(length(d_list) == length(tau_list))

    tau_list2 <- tau_list
    if (combine_priors == "crossed"){
      cnt <- 0
      for (i in seq_along(d_list)){
        for(j in seq_along(tau_list)){
          cnt <- cnt + 1
          d_list2[[cnt]] <- d_list[[i]]
          tau_list2[[cnt]] <- tau_list[[j]]
        }
      }
    }
  }

  # lazy evaluation
  dl <- data_list(model = "random", y = y, SE = SE, labels = labels, data = data,
                  args = as.list(match.call())[-1])[-1]

  # fit meta-analysis with different priors:
  fits <- list()
  for (i in seq_along(d_list2)){
    if (analysis == "fixed"){
      fits[[i]] <- meta_fixed(y = y, SE = SE, labels = labels, data = dl,
                              d = d_list2[[i]], ...)

    } else if (analysis == "random"){
      fits[[i]] <- meta_random(y = y, SE = SE, labels = labels, data = dl,
                               d = d_list2[[i]],
                               tau = tau_list2[[i]], ...)

    } else if (analysis == "bma"){
      fits[[i]] <- meta_bma(y = y, SE = SE, labels = labels, data = dl,
                            d = d_list2[[i]],
                            tau = tau_list2[[i]], ...)
    }
  }

  class(fits) <- "meta_sensitivity"
  fits
}


print_priors <- function(prior_list, digits = 3){

  if (class(prior_list[[1]]) == "meta_bma"){
    prior_d   <- sapply(prior_list, function(x) describe_prior(x$meta$random$prior_d))
    prior_tau <- sapply(prior_list, function(x) describe_prior(x$meta$random$prior_tau))

  } else if (class(prior_list[[1]]) == "meta_random"){
    prior_d   <- sapply(prior_list, function(x) describe_prior(x$prior_d))
    prior_tau <- sapply(prior_list, function(x) describe_prior(x$prior_tau))

  } else if (class(prior_list[[1]]) == "meta_fixed"){
    prior_d <- sapply(prior_list, function(x) describe_prior(x$prior_d))
  }

  cat("Prior distributions on d (= overall effect size):\n\n")
  print(prior_d, digits = digits)

  if (class(prior_list[[1]]) != "meta_fixed"){
    cat("\nPrior distributions on tau (= SD of effect sizes):\n\n")
    print(prior_tau, digits = digits)
  }

}

#' @export
print.meta_sensitivity <- function(x, digits = 3, ...){


  cat("### Sensitivity analysis for Bayesian meta-analysis ###\n\n")

  print_priors(x)


  cat("\nParameter estimates:\n\n")

  estimates <- list()
  for (i in seq_along(x)){
    estimates[[i]] <- cbind("prior" = i, x[[i]]$estimates)
  }
  X <- do.call("rbind", estimates)
  rownames(X) <- paste0(rownames(X), X[,"prior"])
  print(X[sort(rownames(X)),], digits = digits)



  cat("\nPosterior model probabilities:\n\n")

  postprob <- list()
  for (i in seq_along(x)){
    postprob[[i]] <- x[[i]]$posterior_models
  }
  X2 <- do.call("rbind", postprob)
  rownames(X2) <- paste0("prior", seq_along(postprob))
  print(X2, digits = digits)

}

#' Plot Sensitivity Analysis for Meta-Analysis
#'
#' Plot prior or posterior distributions of multiple analyses performed with
#' [meta_sensitivity()].
#'
#' @inheritParams plot.prior
#' @param parameter  which parameter should be plotted: `"d"` or `"tau"`.
#' @param distribution  which distribution should be plotted: `"prior"` or `"posterior"`.
#' @param n integer; the number of x values at which to evaluate.
#' @param legend whether to print all prior specifications and plot a corresponding legend.
#'
#' @details
#' For meta-analysis with model averaging via [meta_bma()], plotting the
#' model-averaged posterior of `tau` is not yet supported. Instead, the posterior
#' distributions for the random effects models will be plotted.
#'
#' @seealso [meta_sensitivity()]
#' @method plot meta_sensitivity
#' @export
plot.meta_sensitivity <- function(x,
                                  parameter = "d",
                                  distribution = "posterior",
                                  from,
                                  to,
                                  n = 101,
                                  legend = TRUE,
                                  ...){

  parameter <- match.arg(parameter, c("d", "tau"))
  distribution <- match.arg(distribution, c("prior", "posterior"))


  dens_label <- paste0(distribution, "_", parameter)
  model <- switch(class(x[[1]]),
                  "meta_bma" = "Model-averaged",
                  "meta_fixed" = "Fixed-effects",
                  "meta_random" = "Random-effects")

  ### get densities

  dens_list <- list()
  for (i in seq_along(x)){
    dens_list[[i]] <- x[[i]][[dens_label]]

    if (class(x[[i]]) == "meta_bma" && parameter == "tau"){
      if (i ==1)
        warning("Model-averaged posterior of parameter tau currently not supported.\n",
                "Showing the posterior of the random effects meta-analyses instead.")
      dens_list[[i]] <- x[[i]]$meta$random[[dens_label]]
      model <- "Random-effects"
    }

    # for meta_bma: list of densities
    if (class(dens_list[[i]]) == "list")
      dens_list[[i]] <- dens_list[[i]][[1]]
  }

  ### plotting

  if (missing(from))
    from <- ifelse(parameter == "tau", 0, -1)
  if (missing(to))
    to <- 1
  stopifnot(length(from) == 1, length(to) == 1, from < to)

  xx <- seq(from, to, length.out = 101)
  dx <- sapply(dens_list, function(d) d(xx))

  Distribution <- paste(model, chartr("p", "P", distribution))

  plot(xx, dx[,1], type = "l", ylim = c(0, max(dx)),
       xlab = paste("Parameter", parameter),
       ylab = paste(chartr("p", "P", distribution), "density"),
       main = paste("Sensitivity:", Distribution),
       las = 1) #, ...)
  abline(v = 0, col = "gray50")
  for(i in 2:length(x)){
    curve(dens_list[[i]](x), add = TRUE, col = i, lty = 1 + ((i-1) %% 5))
  }

  stopifnot(is.logical(legend))
  if (legend){
    legend("topright",
           title = "Prior",
           legend = seq_along(x),
           col = seq_along(x),
           lty = 1 + (seq_along(x) %% 5))
    print_priors(x)
  }
}

# summary of pairwise Bayes factors:
# bfs <- sapply(fits, "[[", "BF")
# rownames(bfs) <- outer(rownames(fits[[1]]$BF), colnames(fits[[1]]$BF), paste)
# bfs
#
# # summary of inclusion Bayes factor:
# bfs2 <- sapply(fits, function(f) f$inclusion$incl.BF)
# bfs2
