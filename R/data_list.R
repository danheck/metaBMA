### Make data-list structure and check data
data_list <- function (model,
                       y, SE, labels = NULL,
                       d = NULL,
                       d.par = NULL,
                       tau = NULL,
                       tau.par = NULL,
                       prior = NULL){

  if (missing(labels) || is.null(labels))
    labels <- paste("Study", 1:length(eval(y)))

  if(any(SE <= 0))
    stop ("Non-positive study standard deviation SE!")

  if (length(y) != length(SE) || length(y) != length(labels))
    stop ("Length of input arguments y, SE, and labels does not match.")

  data_list <- list("model" = model,
                    "y" = y, "SE" = SE, "labels" = labels,
                    "prior.d" = prior(d, d.par, "d")) #list("name" = d, "par" = d.par))

  if (model != "fixed"){
    data_list$prior.tau = prior(tau, tau.par, "tau") #list("name" = tau, "par" = tau.par)
  }else{
    data_list$prior.tau = prior("0", NA, "tau") #list("name" = "0", "par" = NULL)
  }
  if (model == "bma")
    data_list$prior.models = prior/sum(prior)

  return (data_list)
}
