import json, pathlib, os, shlex
import numpy as np
import itertools
import pathlib
from tqdm import tqdm
from collections.abc import Iterable
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import arviz as az
from types import SimpleNamespace

def load_json(path): 
    with open(path, "r") as f: return json.load(f)
    
def no_string_values(x): return {
    key: value 
    for key, value in x.items()
    if not isinstance(value, str)
}

def jsonable(x):
    if isinstance(x, dict): return dict(zip(map(jsonable, x.keys()), map(jsonable, x.values())))
    if isinstance(x, list): return list(map(jsonable, x))
    if isinstance(x, tuple): return tuple(map(jsonable, x))
    if isinstance(x, str): return x
    if isinstance(x, Iterable): return jsonable(x.tolist())
    # if isinstance(x, np.ndarray): return jsonable(x.tolist())
    if isinstance(x, np.int64): return int(x)
    return x

def prepare(path): 
    pathlib.Path(path).parent.mkdir(parents=True, exist_ok=True)

def write_json(path, data):
    prepare(path)
    content = json.dumps(jsonable(data))
    with open(path, "w") as f: f.write(content)
def identity(x): return x
def auto_snakemake(snakemake, fn, post=identity):
    rv = fn(snakemake.input, snakemake.output, **snakemake.wildcards)
    if rv is None: return
    write_json(snakemake.output[0], post(dict(**snakemake.wildcards) | rv))
    return rv

def quantiles(x, qs=[.01,.05,.25,.5,.75,.95,.99], **kwargs): return [np.quantile(x, q, **kwargs) for q in qs]
