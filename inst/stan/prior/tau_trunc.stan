if (tau_family == 1){
  tau_const = - log_diff_exp(normal_lcdf(tau_bnd[2] | tau_param[1], tau_param[2]),
                             normal_lcdf(tau_bnd[1] | tau_param[1], tau_param[2]));
} else if (tau_family == 2) {
  tau_const = - log_diff_exp(student_t_lcdf(tau_bnd[2] | tau_param[3], tau_param[1], tau_param[2]),
                             student_t_lcdf(tau_bnd[1] | tau_param[3], tau_param[1], tau_param[2]));
} else if (tau_family == 3) {
  tau_const = - log(tau_bnd[2] - tau_bnd[1]);  // scaled beta
} else if (tau_family == 4) {
  tau_const = - log_diff_exp(inv_gamma_lcdf(tau_bnd[2] | tau_param[1], tau_param[2]),
                             inv_gamma_lcdf(tau_bnd[1] | tau_param[1], tau_param[2]));
}
