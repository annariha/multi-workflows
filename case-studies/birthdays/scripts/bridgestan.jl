using Pkg
Pkg.activate(pwd())
include(joinpath(pwd(), "scripts/utils.jl"))

function handle(input, output; kwargs...)
    BridgeStan.compile_model(input[1]; stanc_args=["--include-paths=$(abspath(dirname(input[2])))"])
    nothing
end

auto_snakemake(snakemake, handle)