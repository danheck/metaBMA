if (d_family == 1){
  d_const = - log_diff_exp(normal_lcdf(d_bnd[2] | d_param[1], d_param[2]),
                           normal_lcdf(d_bnd[1] | d_param[1], d_param[2]));
} else if (d_family == 2) {
  d_const = - log_diff_exp(student_t_lcdf(d_bnd[2] | d_param[3], d_param[1], d_param[2]),
                           student_t_lcdf(d_bnd[1] | d_param[3], d_param[1], d_param[2]));
} else if (d_family == 3) {
  d_const = - log(d_bnd[2] - d_bnd[1]);  // scaled beta
} else if (d_family == 4) {
  d_const = - log_diff_exp(inv_gamma_lcdf(d_bnd[2] | d_param[1], d_param[2]),
                           inv_gamma_lcdf(d_bnd[1] | d_param[1], d_param[2]));
}
