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

scale(invariants::AbstractVector) = vcat(scale.(invariants)...)
scale(invariants::NamedTuple) = invariants.doy_raw.log_lambda[305]
treatment(invariants::AbstractVector) = vcat(treatment.(invariants)...)
treatment(invariants::NamedTuple) = demean(invariants.doy_raw.weights)[305]

function handle(input, output; model, kwargs...)
    model_fn = getproperty(Main, Symbol(model))
    d1, d2 = JSON.parsefile(input[1])["stan"], JSON.parsefile(input[2])["stan"]
    m1, m2 = model_fn(d1), model_fn(d2)
    f1, f2 = JSON.parsefile.(input[3:end])
    draws1, draws2 = hcat(f1["draws"]...) |> Matrix{Float64}, hcat(f2["draws"]...) |> Matrix{Float64}
    i1, i2 = WarmupHMC.lpdf_and_invariants.([m1, m2], [draws1, draws2])
    s1, s2 = scale.([i1, i2])
    t1, t2 = treatment.([i1, i2])
    qs = [.05, .25, .5, .75, .95]
    return (;
        divergences=(
            noncentered=sum(f1["divergent"]),
            posthoc=sum(f2["divergent"])
        ),
        divergent=(
            noncentered=f1["divergent"],
            posthoc=f2["divergent"],
        ),
        n_steps=(
            noncentered=mean(f1["n_steps"]),
            posthoc=mean(f2["n_steps"])
        ),
        quantiles=(
            noncentered=nanquantile.(Ref(s1), qs),
            posthoc=nanquantile.(Ref(s2), qs)
        ),
        draws=(
            noncentered=(s1, t1),
            posthoc=(s2, t2)
        )
    )
end

auto_snakemake(snakemake, handle)