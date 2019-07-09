#include /auxiliary/license.stan

data {
#include /auxiliary/data.stan
#include /jzs/data.stan
#include /prior/d_data.stan
#include /prior/tau_data.stan
}

transformed data{
  real d_const = 0;
  real tau_const = 0;
#include /auxiliary/se_squared.stan
#include /prior/d_trunc.stan
#include /prior/tau_trunc.stan
}

parameters {
#include /prior/d_param.stan
#include /prior/tau_param.stan
#include /jzs/param.stan
}

model {
#include /prior/d_target.stan
#include /prior/tau_target.stan
#include /jzs/target.stan
  target += normal_lpdf(y | d + X * alpha, sqrt(SE2 + tau^2));
}
