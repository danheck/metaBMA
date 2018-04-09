#include /auxiliary/license.stan

data {
#include /auxiliary/data.stan
#include /JZS/data.stan
#include /prior/d_data.stan
}

transformed data{
#include /prior/d_trunc.stan
}

parameters {
#include /prior/d_param.stan
#include /JZS/param.stan
}

model {
#include /prior/d_target.stan
#include /JZS/target.stan
target += normal_lpdf(y | d + X * alpha, se);
}
