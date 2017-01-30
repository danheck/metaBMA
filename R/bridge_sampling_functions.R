
transform2Real <- function(theta, lb, ub) {

  ### transform samples to real line

  theta_t <- theta
  transTypes <- character()
  cn <- colnames(theta)

  for (i in seq_len(ncol(theta))) {

    p <- cn[i]

    if (lb[[p]] < ub[[p]] && is.infinite(lb[[p]]) && is.infinite(ub[[p]])) {
      transTypes[[p]] <- "unbounded"
      theta_t[,i] <- theta[,i]
    } else if (lb[[p]] < ub[[p]] && is.finite(lb[[p]]) && is.infinite(ub[[p]])) {
      transTypes[[p]] <- "lower-bounded"
      theta_t[,i] <- log(theta[,i] - lb[[p]])
    } else if (lb[[p]] < ub[[p]] && is.infinite(lb[[p]]) && is.finite(ub[[p]])) {
      transTypes[[p]] <- "upper-bounded"
      theta_t[,i] <- log(ub[[p]] - theta[,i])
    } else if (lb[[p]] < ub[[p]] && is.finite(lb[[p]]) && is.finite(ub[[p]])) {
      transTypes[[p]] <- "double-bounded"
      theta_t[,i] <- qnorm( (theta[,i] - lb[[p]])/(ub[[p]] - lb[[p]]) )
    } else {
      stop("Could not transform parameters, possibly due to invalid lower and/or upper
           prior bounds.")
    }

  }

  colnames(theta_t) <- paste0("trans_", colnames(theta))

  return(list(theta_t = theta_t, transTypes = transTypes))

}

invtransform2Real <- function(theta_t, lb, ub) {

  ### transform transformed samples back to original scales

  theta <- theta_t
  colnames(theta) <- stringr::str_sub(colnames(theta), 7)
  cn <- colnames(theta)

  for (i in seq_len(ncol(theta_t))) {

    p <- cn[i]

    if (lb[[p]] < ub[[p]] && is.infinite(lb[[p]]) && is.infinite(ub[[p]])) {
      theta[,i] <- theta_t[,i]
    } else if (lb[[p]] < ub[[p]] && is.finite(lb[[p]]) && is.infinite(ub[[p]])) {
      theta[,i] <- exp(theta_t[,i]) + lb[[p]]
    } else if (lb[[p]] < ub[[p]] && is.infinite(lb[[p]]) && is.finite(ub[[p]])) {
      theta[,i] <- ub[[p]] - exp(theta_t[,i])
    } else if (lb[[p]] < ub[[p]] && is.finite(lb[[p]]) && is.finite(ub[[p]])) {
      theta[,i] <- pnorm(theta_t[,i])*(ub[[p]] - lb[[p]]) + lb[[p]]
    } else {
      stop("Could not transform parameters, possibly due to invalid lower and/or upper
           prior bounds.")
    }

  }

  return(theta)

}

logJacobian <- function(theta_t, transTypes, lb, ub) {

  ### compute log of Jacobian

  logJ <- matrix(nrow = nrow(theta_t), ncol = ncol(theta_t))
  cn <- stringr::str_sub(colnames(theta_t), 7)

  for (i in seq_len( ncol(theta_t) )) {

    p <- cn[i]

    if (transTypes[[p]] == "unbounded") {
      logJ[,i] <- 0
    } else if (transTypes[[p]] == "lower-bounded") {
      logJ[,i] <- theta_t[,i]
    } else if (transTypes[[p]] == "upper-bounded") {
      logJ[,i] <- theta_t[,i]
    } else if (transTypes[[p]] == "double-bounded") {
      logJ[,i] <- log(ub[[p]] - lb[[p]]) + dnorm(theta_t[,i], log = TRUE)
    }

  }

  return(apply(logJ, 1, sum))

}

run.iterative.scheme <- function(q11, q12, q21, q22, r0, tol) {

  ### run iterative updating scheme (using "optimal" bridge function,
  ### see Meng & Wong, 1996)

  l1 <- q11 - q12 # log(l)
  l2 <- q21 - q22 # log(ltilde)

  lstar <- median(l1)
  n.1 <- length(l1)
  n.2 <- length(l2)
  s1 <- n.1/(n.1 + n.2)
  s2 <- n.2/(n.1 + n.2)
  rold <- -100
  r <- r0
  i <- 1

  e <- as.brob( exp(1) )

  while (abs((r - rold)/r) > tol) {

    # cat(paste0("Iterative scheme iteration: ", i, "\n"))
    rold <- r
    numi <- as.numeric( e^(l2 - lstar)/(s1 * e^(l2 - lstar) + s2 *  r) )
    deni <- as.numeric( 1/(s1 * e^(l1 - lstar) + s2 * r) )
    r <- (n.1/n.2) * sum(numi)/sum(deni)
    i <- i + 1

  }

  logml <- log(r) + lstar
  return(list(logml = logml, niter = i-1))

}

bs_armse <- function(logml, q12, q22, q11, q21) {

  # function that computes an approximate relative mean-squared error for
  # a marginal likelihood estimated via bridge sampling
  # (see Fruehwirth-Schnatter, 2004)

  e <- as.brob( exp(1) )

  ml <- e^(logml)
  g_p <- e^(q12)
  g_g <- e^(q22)
  priorTimesLik_p <- e^(q11)
  priorTimesLik_g <- e^(q21)
  p_p <- priorTimesLik_p/ml
  p_g <- priorTimesLik_g/ml

  N1 <- length(p_p)
  N2 <- length(g_g)
  s1 <- N1/(N1 + N2)
  s2 <- N2/(N1 + N2)

  f1 <- as.numeric( p_g/(s1*p_g + s2*g_g) )
  f2 <- as.numeric( g_p/(s1*p_p + s2*g_p) )
  rho_f2 <- spectrum0.ar( f2 )$spec

  term1 <- 1/N2 * var( f1 ) / mean( f1 )^2
  term2 <- rho_f2/N1 * var( f2 ) / mean( f2 )^2

  re2 <- term1 + term2
  return(re2)

}

bridge.sampler <- function(post.samples = NULL, log.posterior = NULL, data = NULL,
                           lb = NULL, ub = NULL, cores = 1, packages = NULL,
                           r0 = 0, tol = 10^(-10), ...) {

  # see Meng & Wong (1996), equation 4.1

  # post.samples: matrix with posterior samples (colnames need to correspond to parameter names)
  # log.posterior: function for computing log of prior times likelihood
  # data: data
  # lb: named vector with lower bounds for parameters
  # ub: named vector with upper bounds for parameters
  # cores: number of cores used for evaluating log.posterior
  # packages: character vector with names of packages needed for evaluating
  #           log.posterior in parallel (only relevant if cores > 1)
  # r0: initial guess for iterative updating scheme
  # tol: tolerance which is used for determining when to stop the iterative scheme

  # returns a list with the following elements:
  # logml: estimate of log marginal likelihood
  # re2:   estimate of relative mean squared error (see Fruehwirth-Schnatter, 2004)
  # % error: based on re2, assumes that bridge estimate is unbiased;
  #          converts re2 to coefficient of variation and then expresses result as %

  # transform parameters to real line
  tmp <- transform2Real(post.samples, lb, ub)
  theta_t <- tmp$theta_t
  transTypes <- tmp$transTypes

  # split samples for fitting multivariate normal distribution/iterative scheme
  nr <- nrow(post.samples)
  samples4fit_index <- seq_len(nr) %% 2 == TRUE # split samples in even/odd
  samples_4_fit <- theta_t[samples4fit_index, ,drop = FALSE]
  samples_4_iter <- theta_t[!samples4fit_index, , drop = FALSE]
  n_post <- nrow(samples_4_iter)

  # fit multivariate normal distribution and generate samples from it
  m <- apply(samples_4_fit, 2, mean)
  V_tmp <- cov(samples_4_fit)
  V <- as.matrix(nearPD(V_tmp)$mat) # make sure that V is positive-definite
  gen_samples <- rmvnorm(n_post, m, V)

  # evaluate fitted multivariate normal distribution for posterior samples and generated samples
  q12 <- dmvnorm(samples_4_iter, m, V, log = TRUE)
  q22 <- dmvnorm(gen_samples, m, V, log = TRUE)

  # evaluate log likelihood times prior for posterior samples and generated samples
  # if (cores == 1) {

    q11 <- apply(invtransform2Real(samples_4_iter, lb, ub), 1, log.posterior,
                 data = data, ...) +
      logJacobian(samples_4_iter, transTypes, lb, ub)
    q21 <- apply(invtransform2Real(gen_samples, lb, ub), 1, log.posterior,
                 data = data, ...) +
      logJacobian(gen_samples, transTypes, lb, ub)

    # all.equal( apply(invtransform2Real(gen_samples, lb, ub), 1, log.posterior,
    #                  data = data, ...),
    #            apply(invtransform2Real(gen_samples, lb, ub), 1, log_posterior,
    #                  data = data, ...))

  # }
  # else if (cores > 1) {
  #
  #   require(snowfall, quietly = TRUE)
  #   require(rlecuyer, quietly = TRUE)
  #   sfInit(parallel = TRUE, cpus = cores, type = "SOCK")
  #   sfClusterSetupRNG()
  #   sapply(packages, function(x) sfLibrary(x, character.only = TRUE))
  #   sfExportAll()
  #
  #   q11 <- sfApply(invtransform2Real(samples_4_iter, lb, ub), 1, log.posterior,
  #                  data = data, ...) +
  #     logJacobian(samples_4_iter, transTypes, lb, ub)
  #   q21 <- sfApply(invtransform2Real(gen_samples, lb, ub), 1, log.posterior,
  #                  data = data, ...) +
  #     logJacobian(gen_samples, transTypes, lb, ub)
  #
  #   sfStop()
  #
  # }

  # run iterative updating scheme to compute log of marginal likelihood
  tmp <- run.iterative.scheme(q11 = q11, q12 = q12, q21 = q21,
                              q22 = q22, r0 = r0, tol = tol)
  logml <- tmp$logml
  niter <- tmp$niter

  #  compute approximate relative mean-squared error (see Fruehwirth-Schnatter, 2004)
  re2 <- bs_armse(logml = logml, q12 = q12, q22 = q22, q11 = q11, q21 = q21)

  # convert to coefficient of variation (assumes that bridge estimate is unbiased)
  cv <- sqrt(re2)

  # convert to percentage error
  error <- cv*100

  out <- list("logml" = logml, "re2" = re2, "% error" = error, "niter" = niter)

  return(out)

}
