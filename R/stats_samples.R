

stats_samples <- function (samples,
                           parameter = "d"){

  if (is.null(samples)) return (NULL)

  summ <- coda_summary_mat(samples, parameter)
  hpd <- HPDinterval(samples[,parameter])

  c("Mean" = summ$statistics[parameter, "Mean"],
    "Median" = summ$quantiles[parameter, "50%"],
    "SD" = summ$statistics[parameter, "SD"],
    "q025" = summ$quantiles[parameter, "2.5%"],
    "q975" = summ$quantiles[parameter, "97.5%"],
    "HPD95lower" = hpd[,1],
    "HPD95upper" = hpd[,2])
}

coda_summary_mat <- function (samples, parameter = "d"){

  summ <- summary(samples)
  if(is.null(dim(summ))){
    summ$statistics <- matrix(summ$statistics, nrow = 1,
                              dimnames = list(parameter,
                                              names(summ$statistics)))
    summ$quantiles <- matrix(summ$quantiles, nrow = 1,
                             dimnames = list(parameter,
                                             names(summ$quantiles)))
  }
  return (summ)
}
