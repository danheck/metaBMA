prior_pars <- function(prior){
  par_labels <- switch(attr(prior, "family"),
                       "norm" = c("mean", "sd"),
                       "t" = c("location", "scale", "nu"),
                       "invgamma" = c("shape", "scale"),
                       "beta" = c("shape1", "shape2"),
                       "0" = vector("numeric", 0),
                       "custom" = names(attr(prior, "param")))
  par_labels
}

check_prior <- function(prior, lower = -Inf, upper = Inf){
  attr(prior, "family") <- match.arg(attr(prior, "family"), priors())

  stopifnot(class(prior) == "prior")
  stopifnot(attr(prior, "label") %in% c("d", "tau"))
  stopifnot(attr(prior, "lower") >= lower)  # nonnegative parameters (tau)
  stopifnot(attr(prior, "upper") <= upper)  # nonnegative parameters (tau)

  if (attr(prior, "family") == "0"){
    attr(prior, "param") <- c(0,0)
  } else if (attr(prior, "family") != "custom"){
    stopifnot(is.numeric(attr(prior, "param")))
    par_labels <- prior_pars(prior)
    stopifnot(length(attr(prior, "param")) == length(par_labels))
    if (is.null(names(attr(prior, "param")))){
      names(attr(prior, "param")) <- par_labels
    } else {
      stopifnot(all(names(attr(prior, "param")) %in% par_labels))
      attr(prior, "param") <- attr(prior, "param")[par_labels]
    }

    if (attr(prior, "family") != "0")
      stopifnot(attr(prior, "param")[2] >= 0)
    if (attr(prior, "family") %in% c("ingamma", "beta"))
      stopifnot(attr(prior, "param")[1] >= 0)
    if (attr(prior, "family") == "t"){
      stopifnot(attr(prior, "param")[3] > 0)
    }
  } else {
    if (!is.function(attr(prior, "param")))
      stop ("If family=='custom', then 'param' must be a (density) function. See ?metaBMA::prior")
  }

  prior
}

check_y_se <- function (y, SE, labels){
  stopifnot(is.numeric(y))
  stopifnot(is.numeric(SE))
  stopifnot(all(SE >= 0))
  stopifnot(length(y) == length(SE))
  stopifnot(length(y) == length(labels))
}


# check whether data are identical for meta_bma object
check_data_identical <- function(meta_list){
  if (length(meta_list) > 1){
    for (i in seq(2, length(meta_list))){
      stopifnot(identical(unname(meta_list[[1]]$data$y),
                          unname(meta_list[[i]]$data$y)))
      stopifnot(identical(unname(meta_list[[1]]$data$SE),
                          unname(meta_list[[i]]$data$SE)))
      stopifnot(identical(unname(meta_list[[1]]$data$N),
                          unname(meta_list[[i]]$data$N)))
    }
  }
}

# check whether data are identical for meta_bma object
identical_prior <- function(prior_list){
  ident <- TRUE
  if (length(prior_list) > 1){
    for (i in seq(2, length(prior_list))){
      for (a in c("family", "param", "lower", "upper"))
        ident <-
          identical(attr(prior_list[[1]], a),
                    attr(prior_list[[i]], a))
    }
  }
  if (ident){
    return(prior_list[[1]])
  } else{
    ll <- min(sapply(prior_list, attr, "lower"))
    empty <- function(x) rep(-1, length(x))
    attr(empty, "lower") <- ll
    return(empty)
  }
}

# identical.prior <- function(x, y, ...){
#   identical(attr(x, "family"), attr(y, "family")) &&
#     identical(attr(x, "param"), attr(y, "family")) &&
#     identical(attr(x, "family"), attr(y, "family")) &&
#
# }
