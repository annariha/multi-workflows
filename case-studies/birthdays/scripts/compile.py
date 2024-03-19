import cmdstanpy
# cmdstanpy.install_cmdstan()

if __name__ == "__main__":
    model = cmdstanpy.CmdStanModel(stan_file=snakemake.input[0])
