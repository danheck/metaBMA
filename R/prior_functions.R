

jags_prior <- function (prior,
                        label = NULL){
  param <- attr(prior, "param")
  jags <- switch (attr(prior, "family"),
                  "norm" = paste0("~ dnorm(", param[1],",", 1/param[2]^2, ")"),
                  "halfnorm" =  paste0("~ dnorm(", param[1],",", 1/param[2]^2, ")T(0,)"),
                  "truncnorm" = paste0("~ dnorm(", param[3],",", 1/param[4]^2,
                                       ")T(", param[1],",", param[2],")"),

                  "scaledt" =  paste0("~ dt(", param[1], ", 1/", param[2],"^2, ", param[3], ")"),
                  "halft" =  paste0("~ dt(0, 1/", param[1],"^2, ", param[2], ")T(0,)"),

                  "cauchy" =
                    paste0("~ dnorm(0,scale_",label,")\n  ",
                           "scale_",label," ~ dgamma(1/2, ", param[1],"^2/2)  "),
                  "halfcauchy" =
                    paste0("~ dnorm(0,scale_",label,")T(0,)\n  ",
                           "scale_",label," ~ dgamma(1/2, ", param[1],"^2/2)  "),


                  "triangular" = paste0("Triangular distributions are not supported in JAGS. Use 'sample = 0'!"),
                  "beta" = paste0("~ dbeta(", param[1], ",", param[2],")"),
                  "shiftedbeta" =
                    "NULL - SHIFTED BETA NOT YET SUPPORTED (problem: shifting requires new parameter)",

                  "0" = "<- 0",
                  "custom" = paste0("Custom distributions are not supported in JAGS. Use 'sample = 0'!"),

                  stop ("Distribution not supported in JAGS.")
  )

  return (jags)
}

describe_prior <- function (prior){

  if (is.null(prior)) return (NULL)

  param <- attr(prior, "param")
  labels <- switch (attr(prior, "family"),

                    "norm" = paste0("mean=", param[1], ", sd=", param[2]),
                    "halfnorm" =  paste0("mean=", param[1], ", sd=", param[2]),
                    "truncnorm" = paste0("min=", param[1], ", max=", param[2],
                                         "mean=", param[3], ", sd=", param[4]),

                    "scaledt" =  paste0("mean=", param[1], ", sigma=", param[2],", nu=", param[3]),
                    "halft" =  paste0("scale=", param[1],", nu=", param[2]),

                    "cauchy" = paste0("scale=", param[1]),
                    "halfcauchy" =   paste0("scale=", param[1]),

                    "triangular" =  paste0("min=", param[1], ", peak=", param[2], ", max=", param[3]),
                    "beta" =  paste0("alpha=", param[1], ", beta=", param[2]),
                    "shiftedbeta" =paste0("min=", param[1], ", max=", param[2],
                                          ", alpha=", param[3], ", beta=", param[4]),

                    "0" = "H0 (parameter = 0)",
                    "custom" = paste0("user-specified function"),
                    stop ("Distribution not supported.")
  )
  paste0("'", attr(prior, "family"), "' with ", labels)
}

#' @export
print.prior <- function (x, ...){
  cat("Prior density function (class='prior'):", describe_prior(x),"\n")
}


rprior <- function (n, prior){
  if (is.null(prior)) return (NULL)

  param <- attr(prior, "param")
  rr <- switch (attr(prior, "family"),

                "norm" = rnorm(n, param[1], param[2]),
                "truncnorm" = rtruncnorm(n, param[1], param[2],
                                         param[3], param[4]),

                "scaledt" = rst(n, param[1],  param[2], param[3]),
                "halft" = rhalft(n, param[1], param[2]),

                "cauchy" = rcauchy(n, 0, param[1]),
                "halfcauchy" = rhalfcauchy(n, param[1]),

                "triangular" = rtriangular(n, param[1],
                                           param[2], param[3]),
                "beta" = rbeta(n, param[1], param[2]),
                "shiftedbeta" = param[1] + param[2] *
                  rbeta(n, param[3], param[4]),

                "0" = rep(0, length(n)),
                "custom" = {
                  stop("Custom prior not supported.")
                  # lb <- attr(prior, "lower")
                  # ub <- attr(prior, "upper")
                  # px <- function(p) sapply(p, function(pp)
                  #   integrate(prior, lb, pp)$value )
                  # u <- runif(n, lb, ub)
                  #
                  # xx <- seq(max(lb, 0), min(ub, 3), length.out = 201)
                  # cnt <- 1
                  # while (px(xx[1]) > .002){
                  #   cnt <- cnt + 1
                  #   newx <- seq(min(xx)-1, min(xx), length.out = 41)
                  #   xx <- c(newx, xx)
                  # }
                  # cnt <- 1
                  # while (px(max(xx)) < .998){
                  #   cnt <- cnt + 1
                  #   newx <- seq(from = max(xx), to = max(xx)+1, length.out = 41)
                  #   xx <- c(xx, newx)
                  # }
                  # dx <- prior(xx)
                  # px <- cumsum(dx)
                  # qdens <- splinefun(px, xx)
                  # length.interval <- function (start){
                  #   qdens(start+ci)-qdens(start)
                  # }
                  # oo <- optim((1-ci)/2, length.interval, method = "L-BFGS-B",
                  #             lower=min(px), upper=max(px)-ci)
                  # if (log) dx <- log(dx)
                  # return (dx)
                },

                stop("Distribution not supported.")
  )
  return (rr)
}
