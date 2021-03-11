// family: 1=normal, 2=t, 3=beta, 4=invgamma, 5=gamma
if (tau_family == 1){
  if (is_inf(tau_bnd[1]) && is_inf(tau_bnd[2])){
    tau_const = 0;
  } else {
    tau_const = - log_diff_exp(normal_lcdf(tau_bnd[2] | tau_param[1], tau_param[2]),
                             normal_lcdf(tau_bnd[1] | tau_param[1], tau_param[2]));
  }

} else if (tau_family == 2) {
  if (is_inf(tau_bnd[1]) && is_inf(tau_bnd[2])){
    tau_const = 0;
  } else {
    tau_const = - log_diff_exp(student_t_lcdf(tau_bnd[2] | tau_param[3], tau_param[1], tau_param[2]),
                             student_t_lcdf(tau_bnd[1] | tau_param[3], tau_param[1], tau_param[2]));
  }

} else if (tau_family == 3) {
  if (tau_bnd[1] == 0 && tau_bnd[2] == 1){
    tau_const = 0;
  } else {
    tau_const = - log(tau_bnd[2] - tau_bnd[1]);  // scaled beta
  }

} else if (tau_family == 4) {
  if (tau_bnd[1] == 0 && is_inf(tau_bnd[2])){
    tau_const = 0;
  } else {
    tau_const = - log_diff_exp(inv_gamma_lcdf(tau_bnd[2] | tau_param[1], tau_param[2]),
                             inv_gamma_lcdf(tau_bnd[1] | tau_param[1], tau_param[2]));
  }

} else if (tau_family == 5) {
  if (tau_bnd[1] == 0 && is_inf(tau_bnd[2])){
    tau_const = 0;
  } else {
    tau_const = - log_diff_exp(gamma_lcdf(tau_bnd[2] | tau_param[1], tau_param[2]),
                             gamma_lcdf(tau_bnd[1] | tau_param[1], tau_param[2]));
  }
}
