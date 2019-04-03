# y <- towels$logOR
# SE <- towels$SE
# metafor::rma(y, sei = SE)
# meta_random(y,SE)


# used toget starting values
ml_estimates <- function(y, SE, model = "random", normal_noise = 0){
  N <- length(y)
  d <- sum (y * 1/SE^2) / sum(1/SE^2)
  est <- list("d" = d)

  if (grepl("random", model)){
    Q <- sum((y - d)^2 / SE^2)
    tau_tmp <- (Q - (N - 1)) / (sum(1/SE^2) - sum(1/SE^4) / sum(1/SE^2))
    est$tau <- sqrt(max(.0001, tau_tmp))
  }
  if (grepl("_dstudy", model)){
    est$delta <- y - d
  }
  if (normal_noise > 0) {
    est$d <- rnorm(1, est$d, normal_noise)
    if (!is.null(est$tau)) est$tau <- est$tau + abs(rnorm(1, 0, normal_noise))
    if (!is.null(est$delta)) est$delta <- rnorm(N, 0, normal_noise) # est$delta,
  }
  est
}

