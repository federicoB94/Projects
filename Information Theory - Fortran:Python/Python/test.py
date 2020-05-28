# %%
import numpy as np
from simulation import Metropolis
from rbm import *
from hamiltonian import *

# %%
print("**************")
print("DEBUGGING TEST")
print("**************")

print("N = 3 | M = 3 | alpha = 1 | k = N+M+N*M=15")
print("p = # MC samples = 5")
print("Ising 1D with h = 0.1")
print("Learning rate g = 0.5")

# %%
Ai = np.array([-0.05, 0.08, 0.02])
Bj = np.array([0.1, -0.05, -0.08])
Wij = np.array([
    [-0.03,  0.02, 0.1 ],
    [ 0.07, -0.12, 0.03],
    [- 0.1, -0.03, 0.05]
])

print()
print("Ai =" , Ai)
print("Bj =" , Bj)
print("Wij =\n", Wij)

# %%
config, _ = Metropolis(Ai, Bj, Wij, 100, 0, 1)
print()
print("'+1' in Metropolis configurations", np.sum(config ==  1))
print("'-1' in Metropolis configurations", np.sum(config == -1))

# %%
S = np.array([
    [1,  1,  1],
    [1, -1, -1],
    [1, -1, -1],
    [1,  1, -1],
    [1, -1, -1]
])

print()
print("MC spin configurations S =\n", S)

# %%
H = Ising_1D(3, 0.2)
print()
print("Ising hamiltonian =\n", H)

# %%
Eloc = LocalEnergy(S, H, Ai, Bj, Wij)
print("Local Energy =", Eloc)

# %%
lnPsi = logPsiDiff(S[1,:], S[0,:], Ai, Bj, Wij)

print()
print("Log(Psi2) - log(Psi1) =", lnPsi)

# %%
S_kk = np.eye(15)
S_kk[2,2] = -0.5

print()
print("Covariance matrix S_kk =\n", S_kk)

# %%
Fk = np.ones(15)
Fk[7]  = -0.7
Fk[13] = 0.25

print()
print("Forces Fk =", Fk)

# %%
g = 0.5
Ai, Bj, Wij = RBM_update(Ai, Bj, Wij, S_kk, Fk, g)

print()
print("After update:")
print("Ai =" , Ai)
print("Bj =" , Bj)
print("Wij =\n", Wij)