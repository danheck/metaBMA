

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

