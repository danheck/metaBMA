#include /auxiliary/license.stan

data {
#include /auxiliary/data.stan
#include /prior/d_data.stan
#include /prior/tau_data.stan
}

transformed data{
#include /prior/d_trunc.stan
#include /prior/tau_trunc.stan
}

parameters {
#include /prior/d_param.stan
#include /prior/tau_param.stan
vector[N] delta;
}

model {
#include /prior/d_target.stan
#include /prior/tau_target.stan
target += normal_lpdf(delta | 0, tau);
target += normal_lpdf(y | d + delta, se);
}
