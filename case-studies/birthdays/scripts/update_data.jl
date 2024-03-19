using Pkg
Pkg.activate(pwd())
include(joinpath(pwd(), "scripts/utils.jl"))

function handle(input, output; kwargs...)
    model_fn = birthday
    data = JSON.parsefile(input[1])["stan"]
    model = model_fn(data)
    draws = hcat(JSON.parsefile(input[2])["draws"]...) |> Matrix{Float64}
    redata = merge(data, to_string_keys(JSON.parsefile(input[3])))
    remodel = model_fn(redata)
    redraws = WarmupHMC.reparametrize(model, remodel, draws)
    vars = vec(nanvar(redraws, dims=2))
    inits = ReparametrizableDistributions.views(
        ReparametrizableDistributions.Length((
            gp1_intercept_raw=1,
            gp1_sd_raw=1,
            gp1_lengthscale_raw=1,
            gp1_beta_raw=data["gp1_n_functions"],
            gp2_intercept_raw=1,
            gp2_sd_raw=1,
            gp2_lengthscale_raw=1,
            gp2_beta_raw=data["gp2_n_functions"],
            gp3_intercept_raw=1,
            gp3_sd_raw=1,
            gp3_lengthscale_raw=1,
            gp3_beta_raw=data["gp3_n_functions"],
            dow_raw=data["n_dow"],
            doy_parameters_raw=[1,2,2+data["n_doy"]][data["doy_type"]],
            doy_raw=data["n_doy"],
            floating_raw=data["n_floating_groups"],
            sigma_raw=1,
        )), redraws[:, 1]
    )
    inits = map(
        (f, x) -> f(x),
        (
            gp1_intercept_raw=scalarize,
            gp1_sd_raw=scalarize,
            gp1_lengthscale_raw=scalarize,
            gp1_beta_raw=identity,
            gp2_intercept_raw=scalarize,
            gp2_sd_raw=scalarize,
            gp2_lengthscale_raw=scalarize,
            gp2_beta_raw=identity,
            gp3_intercept_raw=scalarize,
            gp3_sd_raw=scalarize,
            gp3_lengthscale_raw=scalarize,
            gp3_beta_raw=identity,
            dow_raw=identity,
            doy_parameters_raw=identity,
            doy_raw=identity,
            floating_raw=identity,
            sigma_raw=scalarize,
        ),
        inits
    )
    (
        stan=no_string_values(redata), sample=(
            iter_warmup=200, adapt_init_phase=0, adapt_metric_window=0, adapt_step_size=200,
            step_size=.1, metric=(inv_metric=vars,), inits=inits, adapt_delta=.999, max_treedepth=14
        )
    )
end

auto_snakemake(snakemake, handle) 