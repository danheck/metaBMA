
# data: list
# ...: => run.jags
get_samples <- function(data,
                        ...){

  parameters <- switch(data$model,
                       "fixed" = "d.fixed",
                       "random" = c("tau")) #, "d"))
  if(data$model == "random" && data$prior.d$name != "0")
    parameters <- c(parameters, "d.random")

  jj <- jags_model(data)
  modelfile <- jj$file

  runjags.options(silent.jags=TRUE, silent.runjags=TRUE)
  suppressWarnings(
    samples <- run.jags(model = modelfile,
                        data = data[c("y","V")],
                        monitor = parameters,
                        summarise = FALSE,
                        ...)
  )

  out <- combine.mcmc(samples)

  return(list(samples = out,
              jagsmodel = jj$model))
}
