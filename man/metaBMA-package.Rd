% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaBMA.R
\docType{package}
\name{metaBMA-package}
\alias{metaBMA}
\alias{metaBMA-package}
\title{metaBMA: Bayesian Model Averaging for Random and Fixed Effects Meta-Analysis}
\description{
Fixed-effects meta-analyses assume that the effect size \eqn{d} is identical
in all studies. In contrast, random-effects meta-analyses assume that effects
vary according to a normal distribution with mean \eqn{d} and standard
deviation \eqn{\tau}. Both models can be compared in a Bayesian framework by
assuming specific prior distribution for \eqn{d} and \eqn{\tau} (see
\code{\link{prior}}). Given the posterior model probabilities, the evidence
for or against an effect (i.e., whether \eqn{d = 0}) and the evidence for or
against random effects can be evaluated (i.e., whether \eqn{\tau = 0}). By
using Bayesian model averaging, both tests can be performed by integrating
over the other model. This allows to test whether an effect exists while
accounting for uncertainty whether study heterogeneity exists (so-called
inclusion Bayes factors). For a primer on Bayesian model-averaged meta-analysis,
see Gronau, Heck, Berkhout, Haaf, and Wagenmakers (2020).
}
\details{
The most general functions in \code{metaBMA} is \code{\link{meta_bma}}, which
fits random- and fixed-effects models, compute the inclusion Bayes factor for
the presence of an effect and the averaged posterior distribution of the mean
effect \eqn{d} (which accounts for uncertainty regarding study
heterogeneity). Prior distributions can be specified and plotted using the
function \code{\link{prior}}.

Moreover, \code{\link{meta_fixed}} and \code{\link{meta_random}} fit a single
meta-analysis models. The model-specific posteriors for \eqn{d} can be
averaged by \code{\link{bma}} and inclusion Bayes factors be computed by
\code{\link{inclusion}}.

Results can be visualized with the functions \code{\link{plot_posterior}},
which compares the prior and posterior density for a fitted meta-analysis,
and \code{\link{plot_forest}}, which plots study and overall effect sizes.

For more details how to use the package, see the vignette:
\code{vignette("metaBMA")}.
}
\section{Funding}{


Funding for this research was provided by the Berkeley Initiative for
Transparency in the Social Sciences, a program of the Center for Effective
Global Action (CEGA), Laura and John Arnold Foundation, and by the German
Research Foundation (grant GRK-2277: Statistical Modeling in Psychology).
}

\references{
Gronau, Q. F., Erp, S. V., Heck, D. W., Cesario, J., Jonas, K. J., & Wagenmakers, E.-J. (2017). A Bayesian model-averaged meta-analysis of the power pose effect with informed and default priors: the case of felt power. Comprehensive Results in Social Psychology, 2(1), 123-138. \doi{10.1080/23743603.2017.1326760}

Gronau, Q. F., Heck, D. W., Berkhout, S. W., Haaf, J. M., & Wagenmakers, E.-J. (2021). A primer on Bayesian model-averaged meta-analysis. Advances in Methods and Practices in Psychological Science, 4(3), 1–19. \doi{10.1177/25152459211031256}

Heck, D. W., Gronau, Q. F., & Wagenmakers, E.-J. (2019).
metaBMA: Bayesian model averaging for random and fixed effects meta-analysis.
\url{https://CRAN.R-project.org/package=metaBMA}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/danheck/metaBMA}
  \item \url{https://danheck.github.io/metaBMA/}
}

}
\author{
Heck, D. W. & Gronau, Q. F.
}
