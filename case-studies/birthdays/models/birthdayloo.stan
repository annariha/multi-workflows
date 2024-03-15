#include functions.stan

data {
    // data
    int n_observations;
    vector[n_observations] x;
    vector[n_observations] y;
    array[n_observations] int day_of_week;
    int n_dow;
    array[n_observations] int day_of_year;
    int n_doy;
    int doy_type;
    int n_floating_groups;
    int n_floating_days;
    array[n_floating_days] int floating_days;
    array[n_floating_days] int floating_day_groups;
    // discretization parameters
    int gp1_n_functions;
    real gp1_boundary_factor;
    int gp2_n_functions;
    real gp2_boundary_factor;
    int gp3_n_functions;
    real gp3_boundary_factor;
    // parametrization parameters
    vector[gp1_n_functions] gp1_shift;
    vector[gp1_n_functions] gp1_centeredness;
    vector[gp2_n_functions] gp2_shift;
    vector[gp2_n_functions] gp2_centeredness;
    vector[gp3_n_functions] gp3_shift;
    vector[gp3_n_functions] gp3_centeredness;
    vector[n_doy] doy_centeredness;
    // convenience
    int likelihood;
    int loo;
}
transformed data {
    vector[gp1_n_functions] gp1_log_eigenvalues = gp_log_eigenvalues(gp1_n_functions, gp1_boundary_factor);
    matrix[n_observations, gp1_n_functions] gp1_X = gp_X(x, gp1_n_functions, gp1_boundary_factor);

    matrix[n_observations, gp2_n_functions] gp2_X = periodic_gp_X(x, gp2_n_functions, gp2_boundary_factor);

    vector[gp3_n_functions] gp3_log_eigenvalues = gp_log_eigenvalues(gp3_n_functions, gp3_boundary_factor);
    matrix[n_observations, gp3_n_functions] gp3_X = gp_X(x, gp3_n_functions, gp3_boundary_factor);

    matrix[n_observations, gp1_n_functions+gp2_n_functions] gp12_X = append_col(gp1_X, gp2_X);
    // Horseshoe
    real nu_global = 100;	   // degrees of freedom for the half-t priors for tau
    real nu_local = 1;       // for the regularized horseshoe
    real slab_scale = 2;     // for the regularized horseshoe
    real slab_df = 100;      // for the regularized horseshoe
    real scale_global = .1;
    real log_slab_scale = log(slab_scale);

    int n_doy_parameters;
    if(doy_type == 1){
        n_doy_parameters = 1;
    }else if(doy_type == 2){
        n_doy_parameters = 2;
    }else if(doy_type == 3){
        n_doy_parameters = 2+n_doy;
    }
}
parameters {
    real gp1_intercept_raw;
    real gp1_sd_raw;
    real gp1_lengthscale_raw;
    vector[gp1_n_functions] gp1_beta_raw;

    real gp2_intercept_raw;
    real gp2_sd_raw;
    real gp2_lengthscale_raw;
    vector[gp2_n_functions] gp2_beta_raw;

    real gp3_intercept_raw;
    real gp3_sd_raw;
    real gp3_lengthscale_raw;
    vector[gp3_n_functions] gp3_beta_raw;

    vector[n_dow] dow_raw;
    vector[n_doy_parameters] doy_parameters_raw;
    vector[n_doy] doy_raw;
    vector[n_floating_groups] floating_raw;
    real sigma_raw;
}
model {
    vector[gp1_n_functions] gp1_scales = gp_scales(gp1_sd_raw, gp1_lengthscale_raw, gp1_log_eigenvalues);
    vector[gp1_n_functions] gp1_beta = transform_hierarchical(gp1_beta_raw, gp1_scales, gp1_centeredness);
    real gp1_intercept = transform_intercept(gp1_intercept_raw, gp1_beta, gp1_shift);
    gp1_intercept ~ normal(0, 1);
    gp1_sd_raw ~ normal(0, 1);
    gp1_lengthscale_raw ~ normal(0, 1);
    gp1_beta_raw ~ noncentered_normal(gp1_scales, gp1_centeredness);

    vector[gp2_n_functions] gp2_scales = periodic_gp_scales(gp2_sd_raw, gp2_lengthscale_raw, gp2_n_functions);
    vector[gp2_n_functions] gp2_beta = transform_hierarchical(gp2_beta_raw, gp2_scales, gp2_centeredness);
    real gp2_intercept = transform_intercept(gp2_intercept_raw, gp2_beta, gp2_shift);
    gp2_intercept ~ normal(0, 1);
    gp2_sd_raw ~ normal(0, 1);
    gp2_lengthscale_raw ~ normal(0, 1);
    gp2_beta_raw ~ noncentered_normal(gp2_scales, gp2_centeredness);

    vector[gp3_n_functions] gp3_scales = gp_scales(gp3_sd_raw, gp3_lengthscale_raw, gp3_log_eigenvalues);
    vector[gp3_n_functions] gp3_beta = transform_hierarchical(gp3_beta_raw, gp3_scales, gp3_centeredness);
    real gp3_intercept = transform_intercept(gp3_intercept_raw, gp3_beta, gp3_shift);
    gp3_intercept ~ normal(0, 1);
    gp3_sd_raw ~ normal(0, 1);
    gp3_lengthscale_raw ~ normal(0, 1);
    gp3_beta_raw ~ noncentered_normal(gp3_scales, gp3_centeredness);

    dow_raw ~ normal(0, 1);
    vector[n_doy] doy_scale;

    if(doy_type == 1){
        doy_parameters_raw[1] ~ normal(0,1);
        doy_scale = rep_vector(exp(doy_parameters_raw[1]), n_doy);
        doy_raw ~ noncentered_normal(doy_scale, doy_centeredness);
    }else if(doy_type == 2){
        doy_parameters_raw[1] ~ normal(3,1);
        doy_parameters_raw[2] ~ normal(0,1);
        doy_scale = rep_vector(exp(doy_parameters_raw[2]), n_doy);
        doy_raw ~ noncentered_student_t(exp(doy_parameters_raw[1]), doy_scale, doy_centeredness);
    }else if(doy_type == 3){
        real log_c = doy_parameters_raw[1];
        real c_aux = exp(2*(log_c-log_slab_scale));
        vector[n_doy] log_lambda = doy_parameters_raw[2:n_doy_parameters-1];
        real log_tau = doy_parameters_raw[n_doy_parameters];
        target += sum(doy_parameters_raw) + doy_parameters_raw[1];
        c_aux ~ inv_gamma(0.5*slab_df, 0.5*slab_df);
        exp(log_lambda) ~ student_t(nu_local, 0, 1);
        exp(log_tau) ~ student_t(nu_global, 0, 2*scale_global);
        doy_scale = exp(log_c + log_lambda - .5 * log_sum_exp(2 * log_c, 2 * (log_tau + log_lambda)) + log_tau);
        doy_raw ~ noncentered_normal(doy_scale, doy_centeredness);
    }
    floating_raw ~ normal(0, 1);
    vector[n_observations] intercept = rep_vector(gp1_intercept, n_observations);
    if(n_dow > 1){
        vector[n_dow] dow = demean(dow_raw);
        if(gp3_n_functions > 0){
            intercept += exp(
                gp3_X * gp3_beta
            ) .* dow[day_of_week];
        }else{
            intercept += dow[day_of_week];
        }
    }
    if(n_doy > 1){
        vector[n_doy] doy = demean(transform_hierarchical(doy_raw, doy_scale, doy_centeredness));
        intercept += doy[day_of_year];
    } 
    intercept[floating_days] = floating_raw[floating_day_groups];

    real sigma = exp(sigma_raw);
    sigma_raw ~ normal(0,1);
  // model
    if(likelihood){
        y ~ normal_id_glm(
            gp12_X,
            intercept,
            append_row(gp1_beta, gp2_beta),
            sigma
        );
        target += -normal_lpdf(
            y[loo] | 
            intercept[loo] + gp12_X[loo] * append_row(gp1_beta, gp2_beta), sigma
        );
    }
}
generated quantities {
    real loo_likelihood;
   {
    vector[gp1_n_functions] gp1_scales = gp_scales(gp1_sd_raw, gp1_lengthscale_raw, gp1_log_eigenvalues);
    vector[gp1_n_functions] gp1_beta = transform_hierarchical(gp1_beta_raw, gp1_scales, gp1_centeredness);
    real gp1_intercept = transform_intercept(gp1_intercept_raw, gp1_beta, gp1_shift);

    vector[gp2_n_functions] gp2_scales = periodic_gp_scales(gp2_sd_raw, gp2_lengthscale_raw, gp2_n_functions);
    vector[gp2_n_functions] gp2_beta = transform_hierarchical(gp2_beta_raw, gp2_scales, gp2_centeredness);
    real gp2_intercept = transform_intercept(gp2_intercept_raw, gp2_beta, gp2_shift);

    vector[gp3_n_functions] gp3_scales = gp_scales(gp3_sd_raw, gp3_lengthscale_raw, gp3_log_eigenvalues);
    vector[gp3_n_functions] gp3_beta = transform_hierarchical(gp3_beta_raw, gp3_scales, gp3_centeredness);
    real gp3_intercept = transform_intercept(gp3_intercept_raw, gp3_beta, gp3_shift);

    vector[n_doy] doy_scale;

    if(doy_type == 1){
        doy_scale = rep_vector(exp(doy_parameters_raw[1]), n_doy);
    }else if(doy_type == 2){
        doy_scale = rep_vector(exp(doy_parameters_raw[2]), n_doy);
    }else if(doy_type == 3){
        real log_c = doy_parameters_raw[1];
        real c_aux = exp(2*(log_c-log_slab_scale));
        vector[n_doy] log_lambda = doy_parameters_raw[2:n_doy_parameters-1];
        real log_tau = doy_parameters_raw[n_doy_parameters];
        doy_scale = exp(log_c + log_lambda - .5 * log_sum_exp(2 * log_c, 2 * (log_tau + log_lambda)) + log_tau);
    }
    vector[n_observations] intercept = rep_vector(gp1_intercept, n_observations);
    if(n_dow > 1){
        vector[n_dow] dow = demean(dow_raw);
        if(gp3_n_functions > 0){
            intercept += exp(
                gp3_X * gp3_beta
            ) .* dow[day_of_week];
        }else{
            intercept += dow[day_of_week];
        }
    }
    if(n_doy > 1){
        vector[n_doy] doy = demean(transform_hierarchical(doy_raw, doy_scale, doy_centeredness));
        intercept += doy[day_of_year];
    } 
    intercept[floating_days] = floating_raw[floating_day_groups];

    real sigma = exp(sigma_raw);
    loo_likelihood = normal_lpdf(
            y[loo] | 
            intercept[loo] + gp12_X[loo] * append_row(gp1_beta, gp2_beta), sigma
        );
    };
}