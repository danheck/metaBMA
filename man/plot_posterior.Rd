% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_posterior.R
\name{plot_posterior}
\alias{plot_posterior}
\title{Plot Posterior Distribution}
\usage{
plot_posterior(
  meta,
  parameter = "d",
  from,
  to,
  summary = c("mean", "hpd"),
  ...
)
}
\arguments{
\item{meta}{fitted meta-analysis model}

\item{parameter}{only for random-effects model: whether to plot \code{"d"} or \code{"tau"}}

\item{from}{lower limit of the x-axis}

\item{to}{upper limit of the x-axis}

\item{summary}{character vector with two values: first, either \code{"mean"}
or \code{"50\%"}; and second, either highest-probability-density interval \code{"hpd"}
or the Bayesian credibility interval \code{"bci"}.}

\item{...}{arguments passed to \code{\link[graphics]{plot}}}
}
\description{
Plot Posterior Distribution
}
\seealso{
\link{meta_bma}, \link{meta_fixed}, \link{meta_random}
}
