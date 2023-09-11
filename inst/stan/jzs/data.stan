int<lower=1> B;                       // number of JZS blocks
array[B] int<lower=1> P;                    // number of predictors per block
array[B,2] int<lower=1,upper=sum(P)> b_idx; // start-end index per JZS block for beta / X cols

vector<lower=0>[B] rscale;            // prior scaling for JZS blocks
matrix[N,sum(P)] X;                   // design matrix  (centered)
array[B] matrix[max(P),max(P)] L;           // lower triangular cholesky of:  C=inverse(covariance(X))
