### Make data-list structure and check data
data_list <- function(
    model,
    y,
    SE,
    labels,
    data,
    args
) {

  model <- match.arg(model, c("random", "fixed", "random_ordered"))

  if (!missing(data) && is.list(data)) {
    if (all(c("model", "N", "y", "SE", "labels", "data", "model.frame") %in% names(data))) {
      data$model <- model
      return(data)
    }
    y <- eval(args$y, data) # y <- eval(substitute(y), data)
    SE <- eval(args$SE, data)
    labels <- eval(args$labels, data)

    if (is.character(y)) y <- data[[y]]
    if (is.character(SE)) SE <- data[[SE]]
    if (!missing(labels) && length(labels) == 1 && is.character(labels)) {
      labels <- data[[labels]]
    }
  } else {
    data <- NULL
  }

  if (inherits(y, "formula")) {
    mf <- model.frame(y, data)
    y <- model.response(mf)
    X <- model.matrix(attr(mf, "terms"), mf)
    if (any(attr(X, "assign") != 0)) {
      model <- paste0(model, "_jzs")
    }
  } else {
    mf <- NULL
  }
  if (missing(labels) || is.null(labels)) {
    labels <- paste("Study", 1:length(eval(SE)))
  }

  check_y_se(y, SE, labels)
  if (grepl("random", model) && length(y) < 2) {
    stop("At least n=2 studies are required for a random-effects meta-analysis.")
  }
  if (grepl("jzs", model) && length(y) < 2) {
    stop("At least n=2 studies are required for a meta-analysis with moderator variables.")
  }

  list(
    "model" = model, "N" = length(SE), "y" = y, "SE" = SE, "labels" = labels,
    "data" = data, "model.frame" = mf
  )
}
