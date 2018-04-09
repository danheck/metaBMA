#include /auxiliary/license.stan

data {
#include /auxiliary/data.stan
#include /prior/d_data.stan
#include /prior/tau_data.stan
}

transformed data{
#include /prior/d_trunc.stan
#include /prior/tau_trunc.stan
#include /auxiliary/se_squared.stan
}

parameters {
#include /prior/d_param.stan
#include /prior/tau_param.stan
}

model {
#include /prior/d_target.stan
#include /prior/tau_target.stan
target += normal_lpdf(y | d, sqrt(se2 + tau^2));
}
