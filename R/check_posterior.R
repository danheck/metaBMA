check_posterior <- function (dpost, meta, parameter = "d"){

  mini <- max(0, attr(dpost, "lower"))
  maxi <- min(10, attr(dpost, "upper"))
  xx <- seq(mini, maxi, length.out = 101)

  if (any(is.na(dpost(xx)))){
    warn <- paste("Posterior distribution could not be approximated numerically.\n",
                  "  A density approximation to the JAGS samples is used instead")
    warning (warn)
    ss <- meta$samples[,parameter]
    if (!is.null(ss)){
      lspline <- logspline(ss, min(ss), max(ss))
      dlog <- function(x, log = FALSE){
        dx <- dlogspline(x, lspline)
        if (log) dx <- log(dx)
        return (dx)
      }
      attr(dlog, "lower") <- attr(dpost, "lower")
      attr(dlog, "upper") <- attr(dpost, "upper")
      class(dlog) <- "posterior"
      attr(dlog, "model") <- attr(dpost, "model")
      return (dlog)
    } else {
      warning("JAGS samples missing.")
    }
  }

  return (dpost)
}
