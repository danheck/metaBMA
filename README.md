[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/metaBMA)](http://cran.r-project.org/package=metaBMA)
[![Build Status](https://travis-ci.org/danheck/metaBMA.svg?branch=master)](https://travis-ci.org/danheck/metaBMA)
[![Licence](https://img.shields.io/badge/licence-GPL--2-green.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![DOI](https://zenodo.org/badge/80750374.svg)](https://zenodo.org/badge/latestdoi/80750374)
[![monthly downloads](http://cranlogs.r-pkg.org/badges/metaBMA)](http://cranlogs.r-pkg.org/badges/metaBMA)
[![total downloads](http://cranlogs.r-pkg.org/badges/grand-total/metaBMA)](http://cranlogs.r-pkg.org/badges/grand-total/metaBMA)


# metaBMA

Fixed-effects meta-analyses assume that the effect size $d$ is identical in all studies. In contrast, random-effects meta-analyses assume that effects vary according to a normal distribution with mean $d$ and standard deviation $\tau$. When assuming prior distributions for $d$ and $\tau$, both models can be compared using Bayes factors. Alternatively, posterior model probabilities can be used to compare the evidence for or against an effect (i.e., whether $d = 0$) and the evidence for or against random effects (i.e., whether $\tau = 0$). By using Bayesian model averaging (BMA), both types of tests can be performed by marginalizing over the other question. Most importantly, this allows to test whether an effect exists while accounting for uncertainty whether study heterogeneity exists or not.


## Installing metaBMA

To install the latest stable release of `metaBMA` from CRAN, run:

```r
install.packages("metaBMA")
```

The latest developer version of `metaBMA` can be installed from GitHub via:

```r
### install dependencies if necessary:
# install.packages(c("rstan", "rstantools", "bridgesampling",
#                    "LaplacesDemon", "logspline", "mvtnorm",
#                    "coda", "knitr", "methods"))

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("danheck/metaBMA", ref = "dev")
```

Note that `metaBMA` requires the software [Stan](http://mc-stan.org/). 
In case of issues with using Stan, information how to install the R package `rstan` is available here:
https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started


## Getting Started

The most general functions in `metaBMA` are `meta_bma` and `meta_default`, which fit random- and fixed-effects models, compute the inclusion Bayes factor for the presence of an effect and the averaged posterior distribution of the mean effect $d$ (which accounts for uncertainty regarding study heterogeneity).

Moreover, `meta_fixed()` and `meta_random()` fit standard meta-analysis models with fixed-effects and random-effects, respectively. The model-specific posteriors for the parameter d can be averaged with `bma()` and inclusion Bayes factors be computed with `inclusion()`. 

The function `prior()` facilitates the construction and visual inspection of prior distributions. Sensitivity analysis can be performed with the function `meta_sensitivity()`.

For an overview, see: https://danheck.github.io/metaBMA/


## References

If you use `metaBMA`, please cite the software as follows:

Heck, D. W., Gronau, Q. F., & Wagenmakers, E.-J. (2019). 
metaBMA: Bayesian model averaging for random and fixed effects meta-analysis. https://CRAN.R-project.org/package=metaBMA

An (open-access) introduction to Bayesian meta-analysis with model averaging is available at:

Gronau, Q. F., Heck, D. W., Berkhout, S. W., Haaf, J. M., & Wagenmakers, E.-J. (2021). 
A primer on Bayesian model-averaged meta-analysis. 
*Advances in Methods and Practices in Psychological Science, 4*, 1–19. 
https://doi.org/10.1177/25152459211031256

