

resampling <- function(meta,
                       resample = 5000){
  meta$samples <- sample(meta$samples, resample, TRUE)
  return(meta)
}



summary_ests <- function(meta, parameter = NULL, ...){
  if(is.vector(meta) || is.matrix(meta))
    meta <- list(samples = as.matrix(meta))

  if(missing(parameter) || is.null(parameter)){
    parameter <- 1
  }else if(! parameter %in% colnames(meta$samples) ){
    return(NULL)
  }

  ss <- meta$samples[, parameter ]
  hpd <- HPDinterval(as.mcmc(ss ))
  names(hpd) <- c("HPD95_lower", "HPD95_upper")

  c(Mean = mean(ss, ...),
    Median = median(ss, ...),
    SD = sd(ss,...),
    hpd)
}
