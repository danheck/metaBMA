# use ML estimates to get good starting values
# check:
# y <- towels$logOR
# SE <- towels$SE
# metafor::rma(y, sei = SE)
# meta_random(y,SE)

#' @importFrom LaplacesDemon rtrunc
ml_estimates <- function(y, SE, model = "random", d, tau, normal_noise = 0){
  N <- length(y)
  est <- list("d" = sum (y * 1/SE^2) / sum(1/SE^2))

  if (grepl("random", model)){
    Q <- sum((y - est$d)^2 / SE^2)
    tau_tmp <- (Q - (N - 1)) / (sum(1/SE^2) - sum(1/SE^4) / sum(1/SE^2))
    est$tau <- sqrt(max(.0001, tau_tmp))
  }
  if (grepl("_dstudy", model)){
    est$delta <- y - est$d
  }
  if (normal_noise > 0) {
    est$d <- rtrunc(1, family = "norm", attr(d, "lower"), attr(d, "upper"),
                    mean = est$d, sd = normal_noise)
    if (!is.null(est$tau))
      est$tau <- rtrunc(1, family = "norm", attr(tau, "lower"), attr(tau, "upper"),
                        mean = est$tau, sd = normal_noise)
    if (!is.null(est$delta))
      est$delta <- rnorm(N, 0, normal_noise) # est$delta,
  }
  est
}

