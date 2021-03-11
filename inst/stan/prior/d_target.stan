target += d_const;
if (d_family == 1){
  target += normal_lpdf(d  | d_param[1], d_param[2]);
} else if (d_family == 2) {
  target += student_t_lpdf(d  | d_param[3], d_param[1], d_param[2]);
} else if (d_family == 3) {
  target += beta_lpdf((d - d_bnd[1]) / (d_bnd[2] - d_bnd[1]) | d_param[1], d_param[2]);
} else if (d_family == 4) {
  target += inv_gamma_lpdf(d  | d_param[1], d_param[2]);
} else if (d_family == 5) {
  target += gamma_lpdf(d  | d_param[1], d_param[2]);
}
