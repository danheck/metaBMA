for (b in 1:B){  // independent JZS blocks
  target += inv_gamma_lpdf(g[b] | .5, rscale[b]^2 / 2.);  // power ^ not vectorized
  target += multi_normal_cholesky_lpdf(beta[b_idx[b,1]:b_idx[b,2]] |
                                       rep_vector(0, P[b]),
                                       sqrt(g[b]) * L[b,1:P[b],1:P[b]]);
}
