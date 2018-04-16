
#' @importFrom bridgesampling bridge_sampler
meta_bridge_sampling <- function(meta, logml = "integrate",
                                 min_samples = 5000, rel.error = .005, ...){

  if (logml == "integrate" && is.na(meta$logml)){
    warning ("Integral for marginal likelihood could not be computed with ?integrate.\n",
             "  MCMC/Stan sampling and bridge_sampler will be used instead.")
    logml <- "stan"
  }

  if (logml == "integrate")
    return(meta)

  if (is.null(meta$stanfit)){
    warning("MCMC/Stan samples required to compute log-marginal likelihood with bridge sampling!")
    return(meta)
  }

  # refit stan and use bridge_sampler
  nsamples <- length(extract(meta$stanfit)[[1]])
  if (nsamples < min_samples){
    nsamples <- min_samples
    args <- c(list(data_list = meta$data,
                   d = meta$prior_d, tau = meta$prior_tau, jzs = meta$jzs),
              list(...))
    if (is.null(args$warmup)) args$warmup <- 1000
    args$iter <- args$warmup + min_samples

    warning("Only : ", nsamples, " MCMC/Stan samples available.",
            "\n  To increase precision, model is refitted with:  iter=", min_samples+1000, ",warmup=1000)")

    meta$stan_messages <- capture.output(
      meta$stanfit <- do.call("meta_stan", args))
  }

  bs <- bridge_sampler(meta$stanfit, silent = TRUE, use_neff = FALSE)
  em <- error_measures(bs)
  if (em$cv > rel.error)
    warning("Relative error of log-marginal likelihood with bridge sampling: ", em$percentage,
            "\n  (to increase precision, refit with:  iter>", min_samples + 1000, ",warmup=1000)")
  meta$logml <- bs$logml

  meta
}
