# %%
import argparse
from os import remove
import json
import numpy as np
import matplotlib.pyplot as plt
from simulation import Metropolis
from rbm import *
from hamiltonian import *
from lanczos import Lanczos



# %%
parser = argparse.ArgumentParser(description='Ground state search through RBM')

parser.add_argument("--noplot",
                    action="store_true",
                    help="Disable the energy plot")

ising = parser.add_argument_group("Ising Model")
ising.add_argument("--N",
                   type=int,
                   choices=range(1, 15),
                   metavar="[1,14]",
                   required=True,
                   help="Lattice size; if in 2D, it is the length of lattice side and must be in [1,3])")
ising.add_argument("--ll",
                   type=float,
                   default=0.2,
                   help="Strength of self interaction [default=0.2]")
ising.add_argument("--dim2",
                   action="store_const",
                   const=1,   # True
                   default=0,  # False
                   help="Whether to use 2D square lattice instead of 1D [default=False]")

rbm = parser.add_argument_group("RBM settings")
rbm.add_argument("--alpha",
                 type=int,
                 choices=range(1, 1000),
                 metavar='[> 0]',
                 default=2,
                 help="Hidden unit density (integer) [default=2]")
rbm.add_argument("--g",
                 type=float,
                 default=0.1,
                 help="Learning rate [default=0.1]")
rbm.add_argument("--updates",
                 type=int,
                 choices=range(1, int(1e6)),
                 metavar="[> 0]",
                 default=200,
                 help="Number of iterations in the weights update procedure [default=200]")

sim = parser.add_argument_group("Metrpolis settings")
sim.add_argument("--iter",
                 type=int,
                 choices=range(1, int(1e06)),
                 metavar="[> 0]",
                 default=500,
                 help="Number of iterations of the Metropolis algorithm [default=500]")
sim.add_argument("--burnin",
                 type=int,
                 choices=range(0, int(1e06)),
                 metavar="[0, iter-1]",
                 default=450,
                 help="Number of iterations to discard before saving the results [default=450]")
sim.add_argument("--autocorr",
                 type=int,
                 choices=range(1, int(1e06)),
                 metavar="[0, iter-burnin]",
                 default=1,
                 help="Number of iterations between two consecutives records [default=1]")

output = parser.add_argument_group("Output controls")
output.add_argument("--print",
                    type=int,
                    choices=range(1, int(1e06)),
                    metavar="P",
                    default=1,
                    help="Training information will be printed every P iterations [default=1]")
output.add_argument("--out",
                    type=str,
                    default="out.txt",
                    help="File where to store the output")
# %%
args = parser.parse_args()
with open('params.json', 'w') as f:
        json.dump(vars(args), f, indent=4)

if (args.dim2):
    if (args.N > 3):
        print("For square lattice, the maximum size is 3x3; setting to 3")
        args.N = 3
    args.N = args.N**2

if (args.burnin >= args.iter):
    print("'burnin' must be lower than 'iter'; setting to 0")
    args.burnin = 0

if (args.autocorr > args.iter-args.burnin):
    print("'autocorr' must be lower than 'iter' - 'burnin'; setting to 1")
    args.autocorr = 1

M = args.alpha*args.N
if not args.dim2:
    H = Ising_1D(args.N, args.ll)
else:
    H = Ising_2D(np.sqrt(args.N), args.ll)

# %% Ising and init RBM
H = Ising_1D(args.N, args.ll)
Ai, Bj, Wij = RBM_init(args.N, M, 0, 0.01)

eig = Lanczos(H, 200)
print("Ground State (Lanczos) =", eig[0])

Energy = []

# %% weights update
for t in range(args.updates):
    # Metropolis
    config, Opk = Metropolis(Ai, Bj, Wij, args.iter, args.burnin)
    # covariance matrix
    S_kk = Skk(Opk, t+1)
    # local energy
    Eloc = LocalEnergy(config, H, Ai, Bj, Wij)
    # forces
    Fk = Forces(Opk, Eloc)
    # update
    Ai, Bj, Wij = RBM_update(Ai, Bj, Wij, S_kk, Fk, args.g)

    Energy.append(np.real(np.mean(Eloc)))

    if (t+1)%args.print == 0:
        print("Iter = {:3d} | Energy = {}".format(t+1, Energy[-1]) )

# %%
np.savetxt(args.out, Energy)

if not args.noplot:

    gs = eig[0]
    plt.plot(Energy, color="C1", label="RBM")
    plt.axhline(y=gs, xmin=0, xmax=args.updates,
                color="C0", ls="--", lw=1, label="Lanczos")
    plt.text(0, gs+0.1, "E = {:.4f}".format(gs), color="C0")
    plt.legend()
    plt.show()

# %%
