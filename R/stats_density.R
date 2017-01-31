

stats_density <- function (density,
                           lb,
                           ub,
                           ci = .95){
  if (is.null(lb))
    return(NULL)

  # mean
  mean <- integrate(function(x) x*density(x),
                    lb, ub)$value

  # SD
  variance <- integrate(function(x) (x-mean)^2*density(x),
                        lb, ub)$value
  SD <- sqrt(variance)

  ################ quantiles: c(.025, .5, .975)
  tmp <- function(q, p){
    p - integrate(density, lb, q)$value
  }
  interval <- c(max(lb + 1e-30, mean - 5*SD),
                min(mean + 5*SD, ub - 1e-30))
  qq <- sapply(c(.025, .5, .975),
               function(pp) uniroot(tmp, interval, p = pp)$root)

  ################ compute HPD
  xx <- seq(max(lb, qq[1]-SD),
            min(ub, qq[3]+SD), length.out = 501)
  dx <- sapply(xx, density)*diff(xx[1:2])
  px <- cumsum(dx)
  qdens <- splinefun(px, xx)

  length.interval <- function(start){
    qdens(start+ci)-qdens(start)
  }
  oo <- optim((1-ci)/2, length.interval, method = "L-BFGS-B",
              lower=min(px), upper=max(px)-ci)
  hpd <- c(HPD95lower = qdens(oo$par),
           HPD95upper = qdens(oo$par + ci))

  c(Mean = mean, Median = qq[2], SD = SD,
    q025 = qq[1], q975 = qq[3], hpd)
}
