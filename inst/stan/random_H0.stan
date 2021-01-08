#include /include/license.stan

data {
#include /auxiliary/data.stan
#include /prior/tau_data.stan
}

transformed data{
  real tau_const = 0;
#include /auxiliary/se_squared.stan
#include /prior/tau_trunc.stan
}

parameters {
#include /prior/tau_param.stan
}

model {
#include /prior/tau_target.stan
  target += normal_lpdf(y | 0, sqrt(SE2 + tau^2));
}
