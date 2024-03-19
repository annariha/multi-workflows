functions {
    vector demean(vector x){return x - mean(x);}
    vector gp_log_eigenvalues(int M, real L){
        if(M == 0){return rep_vector(0, M);}
        return -0.25*(pi()/2/L)^2 * linspaced_vector(M, 1, M)^2;
    }
    matrix gp_X(vector x, int M, real L){
        if(M == 0){return rep_matrix(x, M);}
        return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
    }
    vector gp_log_scales(real log_sd, real log_lengthscale, vector log_eigenvalues){
        return log_sd + .25 * log(2*pi()) + .5 * log_lengthscale + exp(2*log_lengthscale) .* log_eigenvalues;
    }
    vector gp_scales(real log_sd, real log_lengthscale, vector log_eigenvalues){
        return 1e-8 + exp(gp_log_scales(log_sd, log_lengthscale, log_eigenvalues));
    }
    matrix periodic_gp_X(vector x, int M, real L){
        if(M == 0){return rep_matrix(x, M);}
        return append_col(
            cos(diag_post_multiply(rep_matrix(2*pi()*x/L, M/2), linspaced_vector(M/2, 1, M/2))),
            sin(diag_post_multiply(rep_matrix(2*pi()*x/L, M/2), linspaced_vector(M/2, 1, M/2)))
        );
    }
    vector periodic_gp_log_scales(real log_sd, real log_lengthscale, int M){
        if(M == 0){return rep_vector(0, M);}
        real a = exp(-2*log_lengthscale);
        vector[M/2] q = log_sd + 0.5 * (log(2) - a + to_vector(log_modified_bessel_first_kind(linspaced_int_array(M/2, 1, M/2), a)));
        return append_row(q,q);
    }
    vector periodic_gp_scales(real log_sd, real log_lengthscale, int M){
        return 1e-8 + exp(periodic_gp_log_scales(log_sd, log_lengthscale, M));
    }
    real transform_intercept(real raw, vector weights, vector shifts){
        return raw + dot_product(weights, shifts);
    }
    vector transform_hierarchical(vector raw, vector scales, vector centeredness){
        return raw .* scales .^ (1. - centeredness);
    }
    vector transform_hierarchical(vector raw, real scale, vector centeredness){
        return raw .* scale .^ (1. - centeredness);
    }
    vector transform_hierarchical(vector raw, real location, real scales, vector c1, vector c2){
        return location + (raw - c1 * location) .* scales .^ (1. - c2);
    }
    real noncentered_normal_lpdf(vector raw, real scale, vector centeredness){
        return normal_lpdf(raw | 0, scale .^ centeredness);
    }
    real noncentered_student_t_lpdf(vector raw, real nu, real scale, vector centeredness){
        return student_t_lpdf(raw | nu, 0, scale .^ centeredness);
    }
    real noncentered_student_t_lpdf(vector raw, real nu, vector scale, vector centeredness){
        return student_t_lpdf(raw | nu, 0, scale .^ centeredness);
    }
    real noncentered_normal_lpdf(vector raw, vector scales, vector centeredness){
        return normal_lpdf(raw | 0, scales .^ centeredness);
    }
    real noncentered_normal_lpdf(vector raw, real location, real scales, vector c1, vector c2){
        return normal_lpdf(raw | location .* c1, scales .^ c2);
    }
}