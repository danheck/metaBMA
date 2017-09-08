

stats_density <- function (density,
                           lb = NULL,
                           ub = NULL,
                           ci = .95,
                           rel.tol = .Machine$double.eps^0.8){

  if(class(density) %in% c("prior", "posterior")){
    lb <- attr(density, "lower")
    ub <- attr(density, "upper")
  }
  if (lb == ub)
    return(NULL)

  mean <- NA
  try (mean <- integrate(function(x) x*density(x),
                         lb, ub, rel.tol = rel.tol)$value, silent = TRUE)

  SD <- NA
  try ({
    variance <- integrate(function(x) (x-mean)^2*density(x),
                          lb, ub, rel.tol = rel.tol)$value
    # variance <- integrate(function(x) x^2*density(x),
    #                       lb, ub, rel.tol = rel.tol)$value - mean^2
    SD <- sqrt(variance)}, silent = TRUE)

  ################ quantiles: c(.025, .5, .975)
  qq <- rep(NA, 3)
  try ({
    tmp <- function (q, p){
      p - integrate(density, lb, q, rel.tol = rel.tol)$value
    }
    for (i in c(3, 5, 10, 20, 100, 500, 1000, 5000, 10000)){
      interval <- c(max(lb + 1e-30, mean - i*SD),
                    min(mean + i*SD, ub - 1e-30))
      q.diff <- sapply(interval, tmp, p = c(.025, .5,.975))
      if (all(q.diff[,1] > 0) && all(q.diff[,2] < 0))
        break
    }
    qq <- sapply(c(.025, .5, .975),
                 function (pp) uniroot(tmp, interval, p = pp)$root)
    }, silent = TRUE)

  ################ compute HPD
  hpd <- c("HPD95lower" = NA, "HPD95upper" = NA)
  try ({
    xx <- seq(max(lb, qq[1] - i*SD), min(ub, qq[3] + i*SD), length.out = 201)
    dx <- sapply(xx, density)*diff(xx[1:2])
    px <- cumsum(dx)
    qdens <- splinefun(px, xx)

    length.interval <- function (start){
      qdens(start+ci)-qdens(start)
    }
    oo <- optim((1-ci)/2, length.interval, method = "L-BFGS-B",
                lower=min(px), upper=max(px)-ci)
    hpd <- c("HPD95lower" = qdens(oo$par),
             "HPD95upper" = qdens(oo$par + ci))
  }, silent = TRUE)

  warn <- paste(
    "Some posterior statistics (mean, SD, etc) could not be computed\n",
    " exactly for parameter", attr(density, "label"), "using numerical integration.\n",
    " JAGS estimates will  be shown instead (ensure that 'sample'\n",
    " is set sufficiently large.")
  if (any(is.na(c(mean, qq, SD, hpd))))
    warning(warn)

  c("Mean" = mean, "Median" = qq[2], "SD" = SD,
    "q025" = qq[1], "q975" = qq[3], hpd)
}
