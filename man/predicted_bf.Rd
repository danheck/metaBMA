% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicted_bf.R
\name{predicted_bf}
\alias{predicted_bf}
\title{Predicted Bayes Factors for a New Study}
\usage{
predicted_bf(meta, SE, sample = 100, ...)
}
\arguments{
\item{meta}{model-averaged meta-analysis (fitted with \code{\link{meta_bma}}).}

\item{SE}{a scalar: the expected standard error of future study.
For instance, SE = 1/sqrt(N) for standardized effect sizes and N = sample size)}

\item{sample}{number of simulated Bayes factors}

\item{...}{further arguments passed to rstan::sampling to draw posterior samples for d and tau.}
}
\description{
How much can be learned by an additional study? To judge this, this function
samples the distribution of predicted Bayes factors for a new study given the
current evidence.
}
