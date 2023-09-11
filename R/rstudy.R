rstudy <- function(
    n,
    model,
    param,
    SE
) {

  stopifnot(SE >= 0)
  if (n == 0) {
    return(NULL)
  }

  if (model == "fixed") {
    # fixed H0 / H1
    rr <- rnorm(n, param, SE)
  } else {
    if (is.null(dim(param))) {
      # random H0
      delta <- rnorm(n, 0, param)
    } else {
      # random H1
      delta <- rnorm(n, param[, "d"], param[, "tau"])
    }
    rr <- rnorm(n, delta, SE)
  }

  rr
}
