using Distributions, ReparametrizableDistributions, WarmupHMC, Random, JSON, ReverseDiff, Optim, BridgeStan, NaNStatistics, ParetoSmooth
# , LogDensityProblemsAD, LogDensityProblems, ReverseDiff, Optim, LogExpFunctions, BridgeStan, NaNStatistics, LinearAlgebra

function write_json(json_path, data)
    json_dir = dirname(json_path)
    !isdir(json_dir) && mkpath(json_dir)
    open(json_path, "w") do f
        JSON.print(f, data)
    end
end

function auto_snakemake(snakemake, fn)
    kwargs = (;(
        (Symbol(key), value) for (key, value) in pairs(snakemake.wildcards) if !isa(key, Int) 
    )...)
    rv = fn(getindex.(Ref(snakemake.input), 1:length(snakemake.input)), snakemake.output; kwargs...)
    !isnothing(rv) && write_json(snakemake.output[1], merge(kwargs, rv))
    return rv
end

include("../models/birthday.jl")

import ReparametrizableDistributions: update_dict

centeredness(source) = source.centeredness
centeredness(source::RHS) = source.hierarchy.centeredness

update_nt(::typeof(birthday), source) = (
    gp1_shift=source.prior.gp1.intercept.mean_shift,
    gp1_centeredness=source.prior.gp1.hierarchy.centeredness,
    gp2_centeredness=source.prior.gp2.hierarchy.centeredness,
    gp3_shift=0 .* source.prior.gp3.intercept.mean_shift,
    gp3_centeredness=source.prior.gp3.hierarchy.centeredness,
    doy_centeredness=centeredness(source.prior.doy_raw)
)

WarmupHMC.lpdf_update(source::Product, draw::NamedTuple, lpdf=0.) = begin
    lpdf += logpdf(source, draw.draw)
    (;lpdf)
end

demean(x) = x .- mean(x)

cond_or_nan(X) = try 
    cond(X) 
catch e 
    NaN 
end
hessian_conds(sm::StanModel, draws::AbstractMatrix) = begin 
    eachrow(hcat(hessian_conds.(Ref(sm), eachcol(draws), Ref(nanstd(draws, dims=2)))...))
end
hessian_conds(sm::StanModel, draw::AbstractVector, stds::AbstractMatrix) = begin 
    hess = log_density_hessian(sm, collect(draw))[3]
    return sqrt.(cond_or_nan.([hess, stds .* hess .* stds']))
end

no_string_values(data) = Dict([(key, value) for (key, value) in pairs(data) if !isa(value, AbstractString)])
to_string_keys(data) = Dict([(String(key), value) for (key, value) in pairs(data)])
scalarize(x::AbstractVector) = x[1]

import ReparametrizableDistributions: verify, BridgeStan, LogDensityProblems, lpdf_and_invariants, to_nt, parts

verify(source::ReparametrizableBSLDP, draws::AbstractMatrix) = begin
    println((BridgeStan.param_unc_num(source.proxy), length(source.wrapped))) 
    @assert BridgeStan.param_unc_num(source.proxy) == length(source.wrapped)
    proxy_lpdfs = LogDensityProblems.logdensity.(source, eachcol(draws))
    wrapped_lpdfs = [lpdf_and_invariants(source, draw).lpdf for draw in eachcol(draws)]
    dlpdfs = proxy_lpdfs .- wrapped_lpdfs
    dlpdfs = dlpdfs .- WarmupHMC.nanmean(dlpdfs)
    checks = abs.(dlpdfs) .< 1e-4
    @assert all(checks) """
Failed lpdf check: 
$(dlpdfs[.!checks])
Total: $(sum(checks))
"""
end