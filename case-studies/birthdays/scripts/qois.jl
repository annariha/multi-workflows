using Pkg
Pkg.activate(pwd())
include(joinpath(pwd(), "scripts/utils.jl"))

compute_qois(source, data, fits) = begin
    y = data["y"] |> Vector{Float64} 
    n_obs = length(y)
    n_chains = length(fits)
    draws = vcat(getindex.(fits, Ref("draws"))...) .|> Vector{Float64}
    draws_nt = WarmupHMC.lpdf_and_invariants.(source, draws)
    log_likelihood = hcat([
        logpdf.(Normal.(draw.likelihood.y_mean, draw.likelihood.y_sd), y)
        for draw in draws_nt
    ]...)
    pl = psis_loo(reshape(log_likelihood, (n_obs, :, n_chains)))
    display(pl)
    (
        treatment=compute_treatment.(draws_nt),
        cv_elpd=pl.pointwise(:cv_elpd),
        pareto_k=pl.pointwise(:pareto_k),
        n_divergent=sum([sum(fit["divergent"]) for fit in fits])
    )
end

compute_treatment(draw::NamedTuple) = demean(draw.doy_raw.weights)[305]

function handle(input, output; model, kwargs...)
    model_fn = getproperty(Main, Symbol(model))
    data = JSON.parsefile(input[2])["stan"]
    model = ReparametrizableBSLDP(input[1], model_fn, no_string_values(data))
    fits = JSON.parsefile.(input[3:end])
    # draws = hcat(fit["draws"]...) |> Matrix{Float64}
    draws = vcat(getindex.(fits, Ref("draws"))...)[1:100:end] .|> Vector{Float64}
    ReparametrizableDistributions.verify(model, hcat(draws...))
    compute_qois(model.wrapped, data, fits)
end

auto_snakemake(snakemake, handle)