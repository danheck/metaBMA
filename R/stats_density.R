

stats_density <- function (density,
                           lb = NULL,
                           ub = NULL,
                           ci = .95){

  if(class(density) %in% c("prior", "posterior")){
    lb <- attr(density, "lower")
    ub <- attr(density, "upper")
  }
  if (lb == ub)
    return(NULL)

  mean <- NA
  try (mean <- integrate(function(x) x*density(x),
                         lb, ub)$value, silent = TRUE)

  SD <- NA
  try ({
    variance <- integrate(function(x) (x-mean)^2*density(x),
                          lb, ub)$value
    SD <- sqrt(variance)}, silent = TRUE)

  ################ quantiles: c(.025, .5, .975)
  qq <- rep(NA, 3)
  try ({
    tmp <- function (q, p){
      p - integrate(density, lb, q)$value
    }
    interval <- c(max(lb + 1e-30, mean - 5*SD),
                  min(mean + 5*SD, ub - 1e-30))
    qq <- sapply(c(.025, .5, .975),
                 function (pp) uniroot(tmp, interval,
                                       p = pp)$root)},
    silent = TRUE)

  ################ compute HPD
  hpd <- c("HPD95lower" = NA, "HPD95upper" = NA)
  try ({
    xx <- seq(max(lb, qq[1]-SD),
              min(ub, qq[3]+SD), length.out = 501)
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
