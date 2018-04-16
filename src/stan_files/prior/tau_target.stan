target += tau_const;
if (tau_family == 1){
  target += normal_lpdf(tau  | tau_param[1], tau_param[2]);
} else if (tau_family == 2) {
  target += student_t_lpdf(tau  | tau_param[3], tau_param[1], tau_param[2]);
} else if (tau_family == 3) {
  target += beta_lpdf((tau - tau_bnd[1]) / (tau_bnd[2] - tau_bnd[1]) | tau_param[1], tau_param[2]);
} else if (tau_family == 4) {
  target += inv_gamma_lpdf(tau  | tau_param[1], tau_param[2]);
}
