
#' Plot Posterior Distribution
#'
#' @param meta fitted meta-analysis model
#' @param from lower bound for x-axis
#' @param to upper bound for x-axis
#' @param parameter only for random-effects model: whether to plot \code{"d"} or \code{"tau"}
#' @param ... arguments passed to \code{\link[graphics]{plot}}
#' @export
plot_posterior <- function (meta,
                            from,
                            to,
                            parameter = "d",
                            ...) {
  NextMethod("plot_posterior", meta,
             parameter = parameter, ...)
}

#' @rdname plot_posterior
#' @export
plot_posterior.meta_fixed <- function(meta,
                                      from = 0,
                                      to = 1,
                                      parameter = "d",
                                      ...){

  prior <- log_prior(meta$data$prior.d, log = FALSE)
  post <- function(d)
    post_fixed(d = d, data = meta$data)/exp(meta$logmarginal)

  plot_density(prior, post, xlab = "Fixed-Effects Mean",
               from = from, to = to, ...)
}

#' @rdname plot_posterior
#' @export
plot_posterior.meta_random <- function(meta,
                                       from = 0,
                                       to = 1,
                                       parameter = "d",
                                       ...){

  if(parameter == "d" && meta$data$prior.d$name != "0"){
    prior.d <- log_prior(meta$data$prior.d, log = FALSE)
    post.d <- function(d) post_random_d(d = d, data = meta$data)/exp(meta$logmarginal)#/meta$const["d"]
    xlab = "Random-Effects Mean"
    plot_density(prior.d, post.d, xlab = xlab, from = from, to = to, ...)

  }else{
    prior.tau <- log_prior(meta$data$prior.tau, log = FALSE)
    post.tau <- function(tau)
      post_random_tau(tau = tau, data = meta$data)/exp(meta$logmarginal)#/meta$const["tau"]
    xlab <- "Heterogeneity of Effects"
    plot_density(prior.tau, post.tau, xlab = xlab, from = from, to = to, ...)
  }


}

#' @rdname plot_posterior
#' @export
plot_posterior.meta_bma <- function(meta,
                                    from = c(0, 1),
                                    to = c(0, 1),
                                    parameter = "d",
                                    ...){

}


plot_density <- function(prior,
                         post,
                         from = 0,
                         to = 1,
                         ...){
  xx <- seq(from, to, length.out = 401)
  dpr <- prior(xx)
  dpo <- post(xx)
  yticks <- pretty(c(0, dpo, dpr))
  xticks <- pretty(c(from, to))
  plot(xx, dpr, type = "l", ylab = "Density", yaxs="i",
       ylim =  range(yticks), xlim = range(xticks),
       col = "darkgray", lty = "dashed", las = 1, bty = "o", lwd = 2, ...)
  lines(xx, dpo, lwd=2, ...)
  legend("topright", legend = c("Prior","Posterior"), bty="n",
         col = c("darkgray", "black"), lty = c("dashed", "solid"), lwd = 2)
}

# debug(metaBMA:::plot_posterior.meta_fixed)
# undebug(metaBMA:::plot_density)
# plot_posterior(bma$meta[[2]])
# plot_posterior(bma$meta[[3]])
# plot_posterior(bma$meta[[4]], parameter  = "tau")
