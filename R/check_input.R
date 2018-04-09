
check_prior <- function(par, lower = -Inf){
  stopifnot(is.numeric(par))
  stopifnot(!anyNA(par))
  stopifnot(length(par) == 5)
  stopifnot(par[4] < par[5])
  stopifnot(par[3] != 0)
  stopifnot(par[1] == round(par[1]))
  stopifnot(par[4] >= lower)
}

check_y_SE <- function (y, SE, labels){
  stopifnot(is.numeric(y))
  stopifnot(is.numeric(SE))
  stopifnot(all(SE >= 0))
  stopifnot(length(y) == length(SE))
  stopifnot(length(y) == length(labels))
}
