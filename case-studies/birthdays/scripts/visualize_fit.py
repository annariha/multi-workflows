from utils import *


def handle(input, output, **kwargs):
    # df = pd.DataFrame(map(load_json, input))
    stan_data = load_json(input[0])["stan"]
    fits = list(map(load_json, input[1:]))
    fig, axes = plt.subplots(3, 1, figsize=(16, 9))
    lp, n_steps, divergent = [
        np.array([fit[key] for fit in fits]) for key in ["lp", "n_steps", "divergent"] 
    ]
    # x, y = map(np.array, map(stan_data.get, ["x", "y"]))
    # y_obs, y_gen, doy_gen = map(np.array, map(fit_data.get, ["y_obs", "y_gen", "doy_gen"]))
    # ax = axes[0]
    # for low, high in zip(y_obs, reversed(y_obs)):
    #     ax.fill_between(x, low, high, alpha=.25, color="red")
    # for low, high in zip(y_gen, reversed(y_gen)):
    #     ax.fill_between(x, low, high, alpha=.25, color="blue")
    # ax.scatter(x, y, color="black")

    # ax = axes[1]
    # for low, high in zip(y_gen, reversed(y_gen)):
    #     ax.fill_between(x, y - low, y - high, alpha=.25, color="red")

    # ax = axes[2]
    # for low, high in zip(doy_gen, reversed(doy_gen)):
    #     ax.fill_between(np.arange(len(low)), low, high, alpha=.25, color="red")

    axes[0].plot(lp.T)
    axes[0].set(title=az.ess(lp))
    # fig.suptitle(f"{fit_data['cfg']}: {sum(fit_data['divergent'])}")

    axes[1].plot(n_steps.T)
    axes[1].set(title=np.mean(n_steps, axis=1))

    axes[2].plot(divergent.T)
    axes[2].set(title=np.sum(divergent, axis=1))
    # fig.suptitle(f"{fit_data['cfg']}: {sum(fit_data['divergent'])}")

    plt.savefig(output[0])
    
    
if __name__ == "__main__": auto_snakemake(snakemake, handle)
 