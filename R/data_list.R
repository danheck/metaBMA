### Make data-list structure and check data
data_list <- function (model, y, SE, labels = NULL, data,
                       d = NULL, d.par = NULL, tau = NULL, tau.par = NULL,
                       prior = NULL, args){

  dl <- data_list_eval(model, y, SE, labels, data, args)

  dl$prior.d = prior(d, d.par, "d")
  if (dl$model != "fixed"){
    dl$prior.tau = prior(tau, tau.par, "tau")
  } else {
    dl$prior.tau = prior("0", NA, "tau")
  }
  if (dl$model == "bma")
    dl$prior.models = prior/sum(prior)

  dl
}

data_list_eval <- function (model, y, SE, labels, data, args){

  if (!missing(data) && is.list(data)){
    y <- eval(args$y, data) #y <- eval(substitute(y), data)
    SE <- eval(args$SE, data)
    labels <- eval(args$labels, data)

    if (is.character(y))   y <- data[[y]]
    if (is.character(SE)) SE <- data[[SE]]
    if (!missing(labels) && length(labels) == 1 && is.character(labels))
      labels <- data[[labels]]
  } else {
    data <- NULL
  }

  if (class(y) == "formula"){
    mf <- model.frame(y, data)
    y <- model.response(mf)
  } else {
    mf <- NULL
  }
  if (missing(labels) || is.null(labels))
    labels <- paste("Study", 1:length(eval(SE)))

  check_y_SE(y, SE, labels)

  list(model = match.arg(model, c("random", "fixed")),
       N = length(SE), y = y, se = SE, SE = SE, labels = labels,
       data = data, model.frame = mf)
}

