# metaBMA

Fixed-effects meta-analyses assume that the effect size d is identical in all studies. In contrast, random-effects meta-analyses assume that effects vary according to a normal distribution with mean d and standard deviation tau. Both models can be compared in a Bayesian framework by assuming specific prior distribution for d and tau (see `prior`). Given the posterior model probabilities, the evidence for or against an effect (i.e., whether d = 0) and the evidence for or against random effects can be evaluated (i.e., whether tau = 0). By using Bayesian model averaging, both tests can be performed by integrating over the other question. Specifically, this allows to test whether an effect exists while allowing uncertainty whether study heterogeneity exists or not (so-called inclusion Bayes factors).

The most general functions in `metaBMA` are `meta_bma` and `meta_default`, which fit random- and fixed-effects models, compute the inclusion Bayes factor for the presence of an effect and the averaged posterior distribution of the mean effect d (which accounts for uncertainty regarding study heterogeneity).

Moreover, `meta_fixed` and `meta_random` fit a single meta-analysis models. The model-specific posteriors for d can be averaged by `bma` and inclusion Bayes factors be computed by `inclusion`. Finally, the function `prior` facilitates the construction and visual inspection of prior distributions.

## Installing metaBMA

`metaBMA` requires the software [JAGS](http://mcmc-jags.sourceforge.net/). To install the latest stable release of `metaBMA` from GitHub, run:

```r
### Dependencies:
# install.packages("devtools", "mvtnorm", "Brobdingnag", "runjags", "LaplacesDemon")
library(devtools)
install_github("danheck/metaBMA")
```

## Citation

If you use metaBMA, please cite the software as follows:

Heck, D. W. & Gronau, Q. F. (2017). metaBMA: Bayesian Model Averaging for Random and Fixed Effects Meta-Analysis. (https://github.com/danheck/metaBMA)
