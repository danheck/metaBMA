rstudy <- function (n,
                    model,
                    param,
                    SE){

  if (SE < 0)
    stop ("SD < 0!")
  if (n == 0)
    return (NULL)

  if (model == "fixed"){
    # fixed H0 / H1
    rr <- rnorm(n, param, SE)

  } else {
    if (is.null(dim(param))){
      # random H0
      d.random <- rnorm(n, 0, param)
    } else {
      # random H1
      d.random <- rnorm(n, param[,"d.random"], param[,"tau"])
    }
    rr <- rnorm(n, d.random, SE)
  }
  return (rr)
}
