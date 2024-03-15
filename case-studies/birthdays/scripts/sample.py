from utils import *
import cmdstanpy, time


def handle(input, output, seed, **kwargs):
    model = cmdstanpy.CmdStanModel(exe_file=input[0])
    data_path = input[1]
    seed = int(seed)
    data = load_json(data_path)
    fit = model.sample(
        no_string_values(data["stan"]), seed=seed, show_progress=True, chains=1, refresh=1, **data["sample"]
    )

    df = fit.draws_pd()
    draws = df.filter(regex=("_raw")).to_numpy()
    lp = df.lp__.to_numpy()
    n_steps = df.n_leapfrog__.to_numpy()
    divergent = df.divergent__.to_numpy()

    return dict(
        draws=draws,
        lp=lp,
        n_steps=n_steps,
        divergent=divergent,
    )
    
    
if __name__ == "__main__": auto_snakemake(snakemake, handle)
 