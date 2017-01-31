
integrate_wrapper <- function(samples,
                              # log_posterior,
                              data ,
                              lb,
                              ub,
                              method = "bridge"){

  if(method == "bridge"){
    integral <- bridge.sampler(post.samples = samples,
                               log.posterior = log_posterior,
                               data = data,
                               lb = lb,
                               ub = ub)

  }else if(method == "integrate"){

    if(length(lb) == 1){

      ####### fixed H1
      if(data$model == "fixed" && data$prior.d$name != "0"){
        integral <- integrate(post_fixed, data = data,
                              lb["d.fixed"], ub["d.fixed"],
                              rel.tol = .Machine$double.eps^0.3)

        ####### random H0
      }else if(data$model == "random" && data$prior.d$name == "0"){
        integral <- integrate(post_random, d = 0, data=data,
                              lb["tau"], ub["tau"],
                              rel.tol = .Machine$double.eps^0.3)
      }

    }else{
      ####### random H1

      tryCatch(
        i1 <- integrate(post_random_d, data=data,
                        lb["d.random"], ub["d.random"],
                        rel.tol = .Machine$double.eps^0.3),
        error = i1 <- NULL)
      tryCatch(
        i2 <- integrate(post_random_tau, data=data,
                        lb["tau"], ub["tau"],
                        rel.tol = .Machine$double.eps^0.3),
        error = i2 <- NULL)

      if(is.null(i1) || is.null(i2)){
        stop("Marginal likelihood could not be computed with marginal='integrate'.\n",
             "  Consider switching to marginal='bridge'.")
      }else if( max(i1$abs.error, i2$abs.error) / i1$value > .01){
        warning("Order of integration resulted in different estimates!\n    ",
                i1$value, "  vs.  ", i2$value)
      }
      if(i1$abs.error < i2$abs.error){
        integral <- i1
      }else{
        integral <- i2
      }
    }

    integral$logml <- log(integral$value)
  }

  return(integral)
}
