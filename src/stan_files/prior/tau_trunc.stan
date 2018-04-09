  real const_tau = - log_diff_exp(student_t_lcdf(p_tau[4] | df_tau, p_tau[1], p_tau[2]),
                                  student_t_lcdf(p_tau[3] | df_tau, p_tau[1], p_tau[2]));
