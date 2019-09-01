int<lower=1> B;                       // number of JZS blocks
int<lower=1> P[B];                    // number of predictors per block
int<lower=1,upper=sum(P)> b_idx[B,2]; // start-end index per JZS block for beta / X cols

vector<lower=0>[B] rscale;            // prior scaling for JZS blocks
matrix[N,sum(P)] X;                   // design matrix  (centered)
matrix[max(P),max(P)] L[B];           // lower triangular cholesky of:  C=inverse(covariance(X))
