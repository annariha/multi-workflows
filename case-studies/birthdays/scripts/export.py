from utils import *
from scipy.special import softmax

def cfg_to_dict(cfg):
    return dict(zip(["n1", "n2", "n3", "dow", "floating", "doy", "cfg"], cfg.split("_")))

def copmute_elpd(df):
    ref = np.array(df.cv_elpd[df.elpd_sum.argmax()])
    df.loc[:, "elpd_diff"] = df.cv_elpd.apply(lambda x: np.sum(x - ref))
    df.loc[:, "elpd_diff_se"] = df.cv_elpd.apply(lambda x: np.std(x - ref)*np.sqrt(len(x)))


def handle(input, output, **kwargs):
    df = pd.DataFrame(map(load_json, input))
    components = df.cfg.apply(lambda cfg: cfg.split("_"))
    df = df[[not (c[2] == "32" and c[3] == "0") for c in components]].reset_index(drop=True)
    df.loc[:, "label"] = list(map(lambda idx: f"Model {1+idx//2}", df.index))
    df.loc[:, "treatment_mean"] = df.treatment.apply(np.mean)
    qs = [2.5, 5, 25, 50, 75, 95, 97.5]
    mqs = ["mean"] + [f"q{q}" for q in qs]
    ks = [.7]#, .5, 0]
    for q in qs:
        df.loc[:, f"treatment_q{q}"] = df.treatment.apply(lambda x: np.quantile(x, q/100))
    for k in ks:
        df.loc[:, f"pareto_k > {k}"] = df.pareto_k.apply(lambda x: np.sum(np.array(x) > k))
    df.loc[:, "elpd_sum"] = df.cv_elpd.apply(np.sum)

    df.loc[:, "parametrization"] = df.cfg.apply(lambda x: "noncentered" if x.endswith("noncentered") else "adapted")
    df.loc[:, "cfg"] = df.cfg.apply(lambda x: x.rsplit("_", 1)[0])
    family_dict = {"1": "normal", "2": "student's t", "3": "RHS"}
    df.loc[:, "family"] = df.cfg.apply(lambda x: family_dict[x.split("_")[-2]])
    df.loc[:, "elpd_diff"] = 0.
    df.loc[:, "elpd_diff_se"] = 0.
    for postfix in mqs:
        df.loc[:, f"loo_bb_weight_{postfix}"] = 0.
    for parametrization in df.parametrization.unique():
        sidx = df.parametrization == parametrization
        sdf = df[sidx]
        ref = np.array(sdf.cv_elpd.iloc[sdf.elpd_sum.argmax()])
        cv_elpds = np.array([row for row in sdf.cv_elpd])
        rng = np.random.default_rng()
        dw = rng.dirichlet(np.ones(cv_elpds.shape[1]), 10000)
        delpds = cv_elpds.shape[1] * cv_elpds @ dw.T
        dloobb = softmax(delpds, axis=0)
        print(dloobb.shape)
        df.loc[sidx, "elpd_diff"] = sdf.cv_elpd.apply(lambda x: np.sum(x - ref))
        df.loc[sidx, "elpd_diff_se"] = sdf.cv_elpd.apply(lambda x: np.std(x - ref)*np.sqrt(len(x)))
        df.loc[sidx, "loo_bb_weight_mean"] = np.mean(dloobb, axis=1)
        for q in qs:
            df.loc[sidx, f"loo_bb_weight_q{q}"] = np.quantile(dloobb, q/100, axis=1)
    # ref = np.array(df.cv_elpd[df.elpd_sum.argmax()])
    # df.loc[:, "elpd_diff"] = df.cv_elpd.apply(lambda x: np.sum(x - ref))
    # df.loc[:, "elpd_diff_se"] = df.cv_elpd.apply(lambda x: np.std(x - ref)*np.sqrt(len(x)))
    adf = df[df.parametrization == "adapted"]
    ranking = list(reversed(adf.elpd_diff.to_numpy().argsort()))
    print(ranking)
    print(adf.cfg.to_numpy())
    ranking_dict = dict(zip(adf.cfg.to_numpy()[ranking], np.arange(len(ranking))))
    df.loc[:, "ranking"] = df.cfg.apply(ranking_dict.get)

    cols = ["label", "ranking", "cfg", "family", "parametrization"] + [f"treatment_{q}" for q in mqs] + [f"loo_bb_weight_mean"] + ["elpd_diff", "elpd_diff_se", "n_divergent"] + [f"pareto_k > {k}" for k in ks]
    df = df[cols].sort_values(by="elpd_diff", ascending=False)
    print(df)
    print(df.filter(regex="loo_bb"))
    df.to_csv(output[0])
    # # df = pd.DataFrame([
    # #     cfg_to_dict(row["cfg"]) | dict(n_divergent=int(row["n_divergent"])) | {
    # #         f"treatment{i}": treatmenti
    # #         for i, treatmenti in enumerate(row["treatment"])
    # #     } | {
    # #         f"elpd{i}": elpdi
    # #         for i, elpdi in enumerate(row["cv_elpd"])
    # #     } | {
    # #         f"pareto_k{i}": pareto_ki
    # #         for i, pareto_ki in enumerate(row["pareto_k"])
    # #     }
    # #     for row in map(load_json, input)
    # # ])
    # treatment_df = df.filter(regex="treatment")
    # df.loc[:, "treatment_mean"] = treatment_df.mean(axis="columns")
    # qs = [5, 25, 50, 75, 95]
    # for q in qs:
    #     df.loc[:, f"treatment_q{q}"] = treatment_df.quantile(q/100, axis="columns")

    # df_elpd = df.filter(regex="elpd")
    # df.loc[:, "elpd_sum"] = df_elpd.sum(axis="columns")
    # df.loc[:, "elpd_diff"] = df.elpd_sum - df.elpd_sum.max()
    # ref = df_elpd.iloc[df.elpd_sum.argmax()]
    # df.loc[:, "elpd_diff_se"] = (df_elpd - ref).std(axis="columns")*np.sqrt(len(df_elpd.columns))

    # df.loc[:, "pareto_k"] = (df.filter(regex="pareto_k") > .7).sum(axis="columns")
    # cols = ["n1", "n2", "n3", "dow", "floating", "doy", "cfg"] + ["treatment_mean"] + [f"treatment_q{q}" for q in qs] + ["elpd_diff", "elpd_diff_se", "n_divergent", "pareto_k"]
    # df[cols].sort_values(by="elpd_diff", ascending=False).to_csv(output[0])
    
    
if __name__ == "__main__": 
    snakemake = globals().get("snakemake")
    if snakemake is None: 
        snakemake = SimpleNamespace(
            input=list(pathlib.Path("samples/birthday/").glob("*/qoi.json")), 
            output=["data/multiverse.csv"],
            wildcards=dict()
        )
    auto_snakemake(snakemake, handle)
 