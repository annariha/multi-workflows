from utils import *
import numpy as np

def rescale_to(x, low, high):
    x = x - np.min(x)
    x = x / np.max(x)
    return low + x * (high - low)

def standardize(x): return (x - np.mean(x)) / np.std(x)

def handle(input, output, **kwargs):
    df = pd.read_csv(input[0])

    idxs = np.arange(len(df)) + 1
    memorial_days = idxs[(df.month == 5) & (df.day_of_week == 1) & (df.day >= 25)]
    labor_days = idxs[(df.month == 9) & (df.day_of_week == 1) & (df.day <= 7)]
    labor_days = np.concatenate([labor_days, labor_days+1])
    thanksgiving_days = idxs[(df.month == 11) & (df.day_of_week == 4) & (df.day >= 22) & (df.day <= 28)]
    thanksgiving_days = np.concatenate([thanksgiving_days, thanksgiving_days+1])
    floating_days_grouped = [memorial_days, labor_days, thanksgiving_days]
    day_of_week = df.day_of_week.to_numpy()
    day_of_year = df.day_of_year2.to_numpy()

    # day_of_year = np.full(len(df), 1)
    if kwargs.get("dow", "0") == "0":
        day_of_week = np.full(len(df), 1)
    n1, n2, n3 = map(int, map(kwargs.get, ["n1", "n2", "n3"], [8, 0, 0]))
    if kwargs.get("floating", "0") == "0":
        floating_days_grouped = [[]]

    floating_days = np.concatenate(floating_days_grouped)
    floating_day_groups = np.concatenate([
        np.full(len(group), 1+idx) for idx, group in enumerate(floating_days_grouped)
    ])
    n_doy = np.max(day_of_year)
    births = df.births.to_numpy()
    y = standardize(births if kwargs["likelihood"] == "1" else np.log(births))
    rv = dict(
        n_observations=len(df),
        x=rescale_to(np.arange(len(df)), -1, 1),
        y=y,
        births=births,
        day_of_week=day_of_week,
        n_dow=np.max(day_of_week),
        day_of_year=day_of_year,
        n_doy=n_doy,
        doy_type=int(kwargs.get("doy", "0")),
        n_floating_groups=len(floating_days_grouped),
        n_floating_days=len(floating_days),
        floating_days=floating_days,
        floating_day_groups=floating_day_groups,
        gp1_n_functions=n1,
        gp1_boundary_factor=1.5,
        gp2_n_functions=n2,
        gp2_boundary_factor=2/(len(df)/365.25),
        gp3_n_functions=n3,
        gp3_boundary_factor=1.5,
        gp1_shift=np.zeros(n1),
        gp1_centeredness=np.zeros(n1),
        gp2_shift=np.zeros(n2),
        gp2_centeredness=np.zeros(n2),
        gp3_shift=np.zeros(n3),
        gp3_centeredness=np.zeros(n3),
        doy_centeredness=np.ones(n_doy),
        likelihood=int(kwargs["likelihood"])#1 if kwargs.get("cfg", "noncentered") != "prior" else 0
    )
    return dict(stan=rv, sample=dict())
    
if __name__ == "__main__": auto_snakemake(snakemake, handle)
 