from utils import *
import cmdstanpy, time


def handle(input, output, cfg, **kwargs):
    model = cmdstanpy.CmdStanModel(exe_file=input[0])
    data = load_json(input[1])
    qoi = load_json(input[2])
    pk = np.array(qoi["pareto_k"])
    idxs = np.flatnonzero(pk > .7)
    if len(idxs) == 0 or len(idxs) > 2 or cfg.find("posthoc") == -1:
        return qoi
    for idx in idxs:
        data["stan"]["loo"] = 1+idx
        fit = model.sample(
            no_string_values(data["stan"]), seed=0, show_progress=True, chains=4, refresh=1, **data["sample"]
        )
        qoi["pareto_k"][idx] = 0
        qoi["cv_elpd"][idx] = np.mean(fit.loo_likelihood)
    return qoi

    # df = fit.draws_pd()
    # draws = df.filter(regex=("_raw")).to_numpy()
    # lp = df.lp__.to_numpy()
    # n_steps = df.n_leapfrog__.to_numpy()
    # divergent = df.divergent__.to_numpy()

    # return dict(
    #     draws=draws,
    #     lp=lp,
    #     n_steps=n_steps,
    #     divergent=divergent,
    # )
    
    
if __name__ == "__main__": auto_snakemake(snakemake, handle)
 