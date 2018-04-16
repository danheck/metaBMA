### Make data-list structure and check data
data_list <- function (model, y, se, labels, data, args){

  model <- match.arg(model, c("random", "fixed"))

  if (!missing(data) && is.list(data)){
    if (all(c("model", "N", "y", "se", "labels", "data", "model.frame") %in% names(data))){
      data$model <- model
      return(data)
    }
    y <- eval(args$y, data) #y <- eval(substitute(y), data)
    se <- eval(args$se, data)
    labels <- eval(args$labels, data)

    if (is.character(y)) y <- data[[y]]
    if (is.character(se)) se <- data[[se]]
    if (!missing(labels) && length(labels) == 1 && is.character(labels))
      labels <- data[[labels]]
  } else {
    data <- NULL
  }

  if (class(y) == "formula"){
    mf <- model.frame(y, data)
    y <- model.response(mf)
    X <- model.matrix(attr(mf, "terms"), mf)
    if (any(attr(X, "assign") != 0))
      model <- paste0(model, "_jzs")

  } else {
    mf <- NULL
  }
  if (missing(labels) || is.null(labels))
    labels <- paste("Study", 1:length(eval(se)))

  check_y_se(y, se, labels)

  list("model" = model, "N" = length(se), "y" = y, "se" = se, "labels" = labels,
       "data" = data, "model.frame" = mf)
}

