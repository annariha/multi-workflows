dirname(Base.active_project()) != pwd() && exit(run(`$(pwd())/julia.sh $(@__FILE__)`).exitcode)
include(joinpath(pwd(), "scripts/utils.jl"))

function handle(input, output; idx, kwargs...)
    rv = JSON.parsefile(input[1])
    rv["stan"]
    rv["stan"]["n_observations"] -= 1
    idx = parse(Int64, idx)
    for key in ["x", "y", "day_of_week", "day_of_year"]
        rv["stan"][key] = vcat(rv["stan"][key][1:idx-1], rv["stan"][key][idx+1:end])
    end
    rv
end

auto_snakemake(snakemake, handle) 