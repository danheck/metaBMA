
check_posterior <- function (dpost, meta, parameter = "d"){

  mini <- max(0, attr(dpost, "lower"))
  maxi <- min(10, attr(dpost, "upper"))
  xx <- seq(mini, maxi, length.out = 101)
  dp_const <- NA
  if (attr(dpost, "lower") < attr(dpost, "upper"))
    try(dp_const <- integrate(dpost, attr(dpost, "lower"), attr(dpost, "upper"))$value, silent = TRUE)
  else
    dp_const <- 1
  # if (is.na(dp_const) || abs(dp_const - 1) > .0005)
  #   try({
  #     m <- meta$estimates[names(attr(dpost, "lower")),"Mean"]
  #     sd <- meta$estimates[names(attr(dpost, "lower")),"SD"]
  #     lb <- max(attr(dpost, "lower"), m - 50*sd)
  #     ub <- max(attr(dpost, "lower"), m + 50*sd)
  #     dp_const <- integrate(dpost, lb, ub)$value
  #   }, silent = TRUE)

  if (any(is.na(dpost(xx))) || is.na(dp_const) || abs(dp_const - 1) > .0005){
    warning ("Posterior distribution could not be approximated numerically\n",
             "  (posterior density integrates to: ", dp_const, ")\n",
             "  A density approximation to the JAGS samples is used instead")
    ss <- meta$samples[,parameter]
    if (!is.null(ss)){
      lspline <- logspline(ss, min(ss), max(ss))
      dlog <- function(x, log = FALSE){
        dx <- dlogspline(x, lspline)
        if (log) dx <- log(dx)
        dx
      }
      attr(dlog, "lower") <- attr(dpost, "lower")
      attr(dlog, "upper") <- attr(dpost, "upper")
      class(dlog) <- "posterior"
      attr(dlog, "model") <- attr(dpost, "model")
      return (dlog)
    } else {
      stop ("JAGS samples missing: Argument 'sample' must be larger than zero!")
    }
  }
  dpost
}
