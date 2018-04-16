/*
    metaBMA is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    metaBMA is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with stanpackage.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.17.0

#include <stan/model/model_header.hpp>

namespace model_random_H0_namespace {

using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

typedef Eigen::Matrix<double,Eigen::Dynamic,1> vector_d;
typedef Eigen::Matrix<double,1,Eigen::Dynamic> row_vector_d;
typedef Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic> matrix_d;

static int current_statement_begin__;

stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_random_H0");
    reader.add_event(0, 0, "include", "/auxiliary/license.stan");
    reader.add_event(0, 0, "start", "/auxiliary/license.stan");
    reader.add_event(14, 14, "end", "/auxiliary/license.stan");
    reader.add_event(14, 1, "restart", "model_random_H0");
    reader.add_event(16, 3, "include", "/auxiliary/data.stan");
    reader.add_event(16, 0, "start", "/auxiliary/data.stan");
    reader.add_event(19, 3, "end", "/auxiliary/data.stan");
    reader.add_event(19, 4, "restart", "model_random_H0");
    reader.add_event(19, 4, "include", "/prior/tau_data.stan");
    reader.add_event(19, 0, "start", "/prior/tau_data.stan");
    reader.add_event(22, 3, "end", "/prior/tau_data.stan");
    reader.add_event(22, 5, "restart", "model_random_H0");
    reader.add_event(27, 10, "include", "/auxiliary/se_squared.stan");
    reader.add_event(27, 0, "start", "/auxiliary/se_squared.stan");
    reader.add_event(30, 3, "end", "/auxiliary/se_squared.stan");
    reader.add_event(30, 11, "restart", "model_random_H0");
    reader.add_event(30, 11, "include", "/prior/tau_trunc.stan");
    reader.add_event(30, 0, "start", "/prior/tau_trunc.stan");
    reader.add_event(42, 12, "end", "/prior/tau_trunc.stan");
    reader.add_event(42, 12, "restart", "model_random_H0");
    reader.add_event(45, 15, "include", "/prior/tau_param.stan");
    reader.add_event(45, 0, "start", "/prior/tau_param.stan");
    reader.add_event(46, 1, "end", "/prior/tau_param.stan");
    reader.add_event(46, 16, "restart", "model_random_H0");
    reader.add_event(49, 19, "include", "/prior/tau_target.stan");
    reader.add_event(49, 0, "start", "/prior/tau_target.stan");
    reader.add_event(59, 10, "end", "/prior/tau_target.stan");
    reader.add_event(59, 20, "restart", "model_random_H0");
    reader.add_event(61, 22, "end", "model_random_H0");
    return reader;
}

#include <meta_header.hpp>
 class model_random_H0 : public prob_grad {
private:
    int N;
    vector_d y;
    vector_d se;
    int tau_family;
    vector_d tau_param;
    vector_d tau_bnd;
    double d;
    double tau_const;
    vector_d se2;
public:
    model_random_H0(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }

    model_random_H0(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, random_seed__, pstream__);
    }

    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning

        current_statement_begin__ = -1;

        static const char* function__ = "model_random_H0_namespace::model_random_H0";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        double DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        // initialize member variables
        try {
            current_statement_begin__ = 17;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            current_statement_begin__ = 18;
            validate_non_negative_index("y", "N", N);
            context__.validate_dims("data initialization", "y", "vector_d", context__.to_vec(N));
            validate_non_negative_index("y", "N", N);
            y = vector_d(static_cast<Eigen::VectorXd::Index>(N));
            vals_r__ = context__.vals_r("y");
            pos__ = 0;
            size_t y_i_vec_lim__ = N;
            for (size_t i_vec__ = 0; i_vec__ < y_i_vec_lim__; ++i_vec__) {
                y[i_vec__] = vals_r__[pos__++];
            }
            current_statement_begin__ = 19;
            validate_non_negative_index("se", "N", N);
            context__.validate_dims("data initialization", "se", "vector_d", context__.to_vec(N));
            validate_non_negative_index("se", "N", N);
            se = vector_d(static_cast<Eigen::VectorXd::Index>(N));
            vals_r__ = context__.vals_r("se");
            pos__ = 0;
            size_t se_i_vec_lim__ = N;
            for (size_t i_vec__ = 0; i_vec__ < se_i_vec_lim__; ++i_vec__) {
                se[i_vec__] = vals_r__[pos__++];
            }
            current_statement_begin__ = 20;
            context__.validate_dims("data initialization", "tau_family", "int", context__.to_vec());
            tau_family = int(0);
            vals_i__ = context__.vals_i("tau_family");
            pos__ = 0;
            tau_family = vals_i__[pos__++];
            current_statement_begin__ = 21;
            validate_non_negative_index("tau_param", "3", 3);
            context__.validate_dims("data initialization", "tau_param", "vector_d", context__.to_vec(3));
            validate_non_negative_index("tau_param", "3", 3);
            tau_param = vector_d(static_cast<Eigen::VectorXd::Index>(3));
            vals_r__ = context__.vals_r("tau_param");
            pos__ = 0;
            size_t tau_param_i_vec_lim__ = 3;
            for (size_t i_vec__ = 0; i_vec__ < tau_param_i_vec_lim__; ++i_vec__) {
                tau_param[i_vec__] = vals_r__[pos__++];
            }
            current_statement_begin__ = 22;
            validate_non_negative_index("tau_bnd", "2", 2);
            context__.validate_dims("data initialization", "tau_bnd", "vector_d", context__.to_vec(2));
            validate_non_negative_index("tau_bnd", "2", 2);
            tau_bnd = vector_d(static_cast<Eigen::VectorXd::Index>(2));
            vals_r__ = context__.vals_r("tau_bnd");
            pos__ = 0;
            size_t tau_bnd_i_vec_lim__ = 2;
            for (size_t i_vec__ = 0; i_vec__ < tau_bnd_i_vec_lim__; ++i_vec__) {
                tau_bnd[i_vec__] = vals_r__[pos__++];
            }

            // validate, data variables
            current_statement_begin__ = 17;
            check_greater_or_equal(function__,"N",N,1);
            current_statement_begin__ = 18;
            current_statement_begin__ = 19;
            check_greater_or_equal(function__,"se",se,0);
            current_statement_begin__ = 20;
            check_greater_or_equal(function__,"tau_family",tau_family,1);
            current_statement_begin__ = 21;
            current_statement_begin__ = 22;
            stan::math::check_ordered(function__,"tau_bnd",tau_bnd);
            // initialize data variables
            current_statement_begin__ = 26;
            d = double(0);
            stan::math::fill(d,DUMMY_VAR__);
            stan::math::assign(d,0);
            current_statement_begin__ = 27;
            tau_const = double(0);
            stan::math::fill(tau_const,DUMMY_VAR__);
            stan::math::assign(tau_const,0);
            current_statement_begin__ = 28;
            validate_non_negative_index("se2", "N", N);
            se2 = vector_d(static_cast<Eigen::VectorXd::Index>(N));
            stan::math::fill(se2,DUMMY_VAR__);

            current_statement_begin__ = 29;
            for (int i = 1; i <= N; ++i) {
                current_statement_begin__ = 30;
                stan::math::assign(get_base1_lhs(se2,i,"se2",1), pow(get_base1(se,i,"se",1),2));
            }
            current_statement_begin__ = 31;
            if (as_bool(logical_eq(tau_family,1))) {

                current_statement_begin__ = 32;
                stan::math::assign(tau_const, -(log_diff_exp(normal_cdf_log(get_base1(tau_bnd,2,"tau_bnd",1),get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)),normal_cdf_log(get_base1(tau_bnd,1,"tau_bnd",1),get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)))));
            } else if (as_bool(logical_eq(tau_family,2))) {

                current_statement_begin__ = 35;
                stan::math::assign(tau_const, -(log_diff_exp(student_t_cdf_log(get_base1(tau_bnd,2,"tau_bnd",1),get_base1(tau_param,3,"tau_param",1),get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)),student_t_cdf_log(get_base1(tau_bnd,1,"tau_bnd",1),get_base1(tau_param,3,"tau_param",1),get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)))));
            } else if (as_bool(logical_eq(tau_family,3))) {

                current_statement_begin__ = 38;
                stan::math::assign(tau_const, -(log((get_base1(tau_bnd,2,"tau_bnd",1) - get_base1(tau_bnd,1,"tau_bnd",1)))));
            } else if (as_bool(logical_eq(tau_family,4))) {

                current_statement_begin__ = 40;
                stan::math::assign(tau_const, -(log_diff_exp(inv_gamma_cdf_log(get_base1(tau_bnd,2,"tau_bnd",1),get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)),inv_gamma_cdf_log(get_base1(tau_bnd,1,"tau_bnd",1),get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)))));
            }

            // validate transformed data
            current_statement_begin__ = 26;
            current_statement_begin__ = 27;
            current_statement_begin__ = 28;

            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 46;
            ++num_params_r__;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    ~model_random_H0() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        stan::io::writer<double> writer__(params_r__,params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        if (!(context__.contains_r("tau")))
            throw std::runtime_error("variable tau missing");
        vals_r__ = context__.vals_r("tau");
        pos__ = 0U;
        context__.validate_dims("initialization", "tau", "double", context__.to_vec());
        double tau(0);
        tau = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(get_base1(tau_bnd,1,"tau_bnd",1),get_base1(tau_bnd,2,"tau_bnd",1),tau);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable tau: ") + e.what());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(vector<T__>& params_r__,
                 vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        T__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;

        try {
            // model parameters
            stan::io::reader<T__> in__(params_r__,params_i__);

            T__ tau;
            (void) tau;  // dummy to suppress unused var warning
            if (jacobian__)
                tau = in__.scalar_lub_constrain(get_base1(tau_bnd,1,"tau_bnd",1),get_base1(tau_bnd,2,"tau_bnd",1),lp__);
            else
                tau = in__.scalar_lub_constrain(get_base1(tau_bnd,1,"tau_bnd",1),get_base1(tau_bnd,2,"tau_bnd",1));


            // transformed parameters



            // validate transformed parameters

            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning

            // model body

            current_statement_begin__ = 50;
            lp_accum__.add(tau_const);
            current_statement_begin__ = 51;
            if (as_bool(logical_eq(tau_family,1))) {

                current_statement_begin__ = 52;
                lp_accum__.add(normal_log(tau,get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)));
            } else if (as_bool(logical_eq(tau_family,2))) {

                current_statement_begin__ = 54;
                lp_accum__.add(student_t_log(tau,get_base1(tau_param,3,"tau_param",1),get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)));
            } else if (as_bool(logical_eq(tau_family,3))) {

                current_statement_begin__ = 56;
                lp_accum__.add(beta_log(((tau - get_base1(tau_bnd,1,"tau_bnd",1)) / (get_base1(tau_bnd,2,"tau_bnd",1) - get_base1(tau_bnd,1,"tau_bnd",1))),get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)));
            } else if (as_bool(logical_eq(tau_family,4))) {

                current_statement_begin__ = 58;
                lp_accum__.add(inv_gamma_log(tau,get_base1(tau_param,1,"tau_param",1),get_base1(tau_param,2,"tau_param",1)));
            }
            current_statement_begin__ = 60;
            lp_accum__.add(normal_log(y,d,sqrt(add(se2,pow(tau,2)))));

        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("tau");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        vars__.resize(0);
        stan::io::reader<double> in__(params_r__,params_i__);
        static const char* function__ = "model_random_H0_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        double tau = in__.scalar_lub_constrain(get_base1(tau_bnd,1,"tau_bnd",1),get_base1(tau_bnd,2,"tau_bnd",1));
        vars__.push_back(tau);

        if (!include_tparams__) return;
        // declare and define transformed parameters
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;

        double DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        try {



            // validate transformed parameters

            // write transformed parameters

            if (!include_gqs__) return;
            // declare and define generated quantities



            // validate generated quantities

            // write generated quantities
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng,params_r_vec,params_i_vec,vars_vec,include_tparams,include_gqs,pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "model_random_H0";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "tau";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "tau";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (!include_gqs__) return;
    }

}; // model

}

typedef model_random_H0_namespace::model_random_H0 stan_model;


#endif
