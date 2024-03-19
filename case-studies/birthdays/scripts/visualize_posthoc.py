from utils import *

def handle(input, output, **kwargs):
    # df = pd.DataFrame(map(load_json, input))
    df = pd.DataFrame([
        dict(key=key, i=i, statei=statei)
        # for row in df.itertuples()
        for key, values in load_json(input[0]).items() if (
            isinstance(values, list)
        )
        for i, statei in enumerate(np.atleast_1d(values))
    ])
    sns.relplot(data=df, x="i", y="statei", row="key", kind="scatter", height=5, facet_kws={'sharey': False, 'sharex': False})
    prepare(output[0])
    plt.savefig(output[0])
    

if __name__ == "__main__":
    handle(snakemake.input, snakemake.output, **snakemake.wildcards)
