using Pkg
Pkg.activate(pwd())
include(joinpath(pwd(), "scripts/utils.jl"))

function handle(input, output; model, kwargs...)
    model_fn = getproperty(Main, Symbol(model))
    model = model_fn(JSON.parsefile(input[1])["stan"])
    draws = hcat(JSON.parsefile(input[2])["draws"]...) |> Matrix{Float64}
    println((size(draws), length(model)))
    nt1 = to_nt(model, 1:length(model))
    println(nt1)
    nt2 = map(to_nt, parts(model), nt1)
    println(nt2)
    @assert size(draws, 1) == length(model)
    remodel = WarmupHMC.find_reparametrization(model, draws; strict=true)
    return update_nt(model_fn, remodel)
end

auto_snakemake(snakemake, handle)