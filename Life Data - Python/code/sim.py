# %%
import networkx as nx
import numpy as np
import argparse
from tqdm import tqdm
from Functions import *

# %%
parser = argparse.ArgumentParser()
parser.add_argument("--model"    , type=str  , default="SIR", choices=["SIR", "SIS"]       , help="compartmental model (default=SIR)")
parser.add_argument("--rrm"      , type=str  , default=""   , choices=["", "time", "nodes"], help="RRM model (default='')")
parser.add_argument("--seed"     , type=int  , default=None , help="seed for RRM (default=None)")
parser.add_argument("--remove"   , type=int  , default=None , help="remove nodes with degree less than this value (default=None)")
parser.add_argument("--p_removed", type=float, default=0    , help="probability that a node is removed when it activates")

args = parser.parse_args()
model = args.model
rrm   = args.rrm
seed  = args.seed
n_rm  = args.remove
p_removed = min(max(args.p_removed, 0), 1)
fname = "Simulations/{}_temporal_{}_prem{}.npz".format(model, np.random.randint(10000, 99999), str(p_removed)[2:]) if rrm is '' else "Simulations/{}_temporal_RRM_{}_{}_prem{}.npz".format(model, rrm, seed, str(p_removed)[2:])

# %%
female, male, time = np.loadtxt("network_rocha-etal.csv", unpack=True, comments="#", dtype=np.int16)

# %% disease parameters
n = np.array([1, 3, 6, 9])
params = np.array([n*1e-04, n*1e-03, n*0.01, n*0.1]).flatten()
tmax = 10000 # days
aggregation = 30 # days
rho = 1
t_initial_seed = 20
p_infected = 1e-03
excl_fraction = 5/7

net_list, agg = temporal_network_list(female, male, time, aggregation, randomize=rrm, seed=seed)

# %% remove nodes with conditions
if n_rm is not None:
    fname = fname.replace(".npz", "_rm{}.npz".format(n_rm))
    edges = np.stack((female, male), axis=-1)
    aggregated_network = nx.from_edgelist(edges)
    remove = [node for node, degree in dict(aggregated_network.degree()).items() if degree < n_rm]
    for net in net_list:
        net.remove_nodes_from(remove)
else:
    pass

# %%
print("Model = {}".format(model))
print("RRM   = {}".format(rrm  ))
print("Seed  = {}".format(seed ))
print("File  = {}".format(fname))

# %%
pbar = tqdm(total=len(params)**2)

if model == "SIR":
    beta, mu, time, S, I, R = [], [], [], [], [], []
    for b in params:
        for m in params:
            #print("\nbeta = {:.4f} | mu = {:.4f}".format(b, m))
            s, i, r, t = SIR_temporal(net_list, agg, tmax=tmax, beta=b, mu=m, rho=rho, p_infected=p_infected, p_removed=p_removed, t_initial_seed=t_initial_seed, exclude_fraction=excl_fraction)
            beta.append(b)
            mu  .append(m)
            time.append(t)
            S.append(s)
            I.append(i)
            R.append(r)
        pbar.update()
    pbar.close()
    np.savez_compressed(fname, mu=mu, beta=beta, time=time, S=S, I=I, R=R, rrm=rrm, seed=seed)
elif model == "SIS":
    beta, mu, time, S, I = [], [], [], [], []
    for b in params:
        for m in params:
            #print("\nbeta = {:.4f} | mu = {:.4f}".format(b, m))
            s, i, t = SIS_temporal(net_list, agg, tmax=tmax, beta=b, mu=m, rho=rho, p_infected=p_infected, p_removed=p_removed,t_initial_seed=t_initial_seed, exclude_fraction=excl_fraction)
            beta.append(b)
            mu  .append(m)
            time.append(t)
            S.append(s)
            I.append(i)
        pbar.update()
    pbar.close()
    np.savez_compressed(fname, mu=mu, beta=beta, time=time, S=S, I=I, rrm=rrm, seed=seed)
else:
    pbar.close()
    pass

