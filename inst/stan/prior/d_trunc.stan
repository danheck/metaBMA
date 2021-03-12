// family: 1=normal, 2=t, 3=beta, 4=invgamma, 5=gamma
if (d_family == 1){
  if (is_inf(d_bnd[1]) && is_inf(d_bnd[2])){
    d_const = 0;
  } else {
    d_const = - log_diff_exp(normal_lcdf(d_bnd[2] | d_param[1], d_param[2]),
                             normal_lcdf(d_bnd[1] | d_param[1], d_param[2]));
  }

} else if (d_family == 2) {
  if (is_inf(d_bnd[1]) && is_inf(d_bnd[2])){
    d_const = 0;
  } else {
    d_const = - log_diff_exp(student_t_lcdf(d_bnd[2] | d_param[3], d_param[1], d_param[2]),
                             student_t_lcdf(d_bnd[1] | d_param[3], d_param[1], d_param[2]));
  }

} else if (d_family == 3) {
  if (d_bnd[1] == 0 && d_bnd[2] == 1){
    d_const = 0;
  } else {
    d_const = - log(d_bnd[2] - d_bnd[1]);  // scaled beta
  }

} else if (d_family == 4) {
  if (d_bnd[1] == 0 && is_inf(d_bnd[2])){
    d_const = 0;
  } else {
    d_const = - log_diff_exp(inv_gamma_lcdf(d_bnd[2] | d_param[1], d_param[2]),
                             inv_gamma_lcdf(d_bnd[1] | d_param[1], d_param[2]));
  }

} else if (d_family == 5) {
  if (d_bnd[1] == 0 && is_inf(d_bnd[2])){
    d_const = 0;
  } else {
    d_const = - log_diff_exp(gamma_lcdf(d_bnd[2] | d_param[1], d_param[2]),
                             gamma_lcdf(d_bnd[1] | d_param[1], d_param[2]));
  }
}
