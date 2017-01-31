
# d, tau: numeric vector
# data: list with y, V, prior.d, prior.tau


posterior <- function(meta,
                      parameter = "d"){

  if(class(meta) == "meta_bma"){
    bf <- meta$BF$H1_fixed_vs_random
    w <- bf/(bf + 1)
    function(d)
      w*posterior(meta$meta$fixed.H1)(d) +
      (1-w)*posterior(meta$meta$random.H1)(d)

  }else if(meta$data$model == "fixed"){
    function(d)
      post_fixed(d = d, data = meta$data)/exp(meta$logmarginal)

  }else if(parameter == "d"){
    function(d)
      post_random_d(d = d, data = meta$data)/exp(meta$logmarginal)

  }else if (parameter == "tau"){
    function(tau)
      post_random_tau(tau = tau, data = meta$data)/exp(meta$logmarginal)
  }else{
    function(tau, d)
      post_random(tau = tau, d = d, data = meta$data)/exp(meta$logmarginal)
  }
}
