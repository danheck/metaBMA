#include /include/license.stan

data {
#include /auxiliary/data.stan
#include /prior/d_data.stan
}

transformed data{
  real d_const = 0;
#include /prior/d_trunc.stan
}

parameters {
#include /prior/d_param.stan
}

model {
#include /prior/d_target.stan
  target += normal_lpdf(y | d, SE);
}
