# %%
import numpy as np
from others import *

# %% inittilize RBM
def RBM_init(N, M, mean=0, std=0.01):
    """
    Randomly initializes the weights of the RBM,
    with normal distribution

        Parameters
        ----------
        N : int
            number of visible units
        M : INTEGER
            number of hidden units
        mean : float [default=0]
            Mean of the normal distribution
        std : float [default=1]
            Standard deviation of the normal distribution

        Returns
        -------
        Ai : np.array(), dim=N
            Biases of the visible units; the subroutine allocates the space.
        Bj : np.array(), dim=M
            Biases of the hidden units; the subroutine allocates the space.
        Wij : np.array(), dim=(N,M)
            Weights between visible and hidden units; the subroutine allocates the space.
    """
    Ai  = np.random.normal(mean, std, N)     + 1j*np.random.normal(mean, std, N)
    Bj  = np.random.normal(mean, std, M)     + 1j*np.random.normal(mean, std, M)
    Wij = np.random.normal(mean, std, (N,M)) + 1j*np.random.normal(mean, std, (N,M))

    return Ai, Bj, Wij

# %% RBM weights update
def RBM_update(Ai, Bj, Wij, S_kk, Fk, g=0.1):
    """
    Update the weights of the RBM.

        Parameters
        ----------
        Ai : np.array(), dim=N
            Biases of the visible units
        Bj : np.array(), dim=M
            Biases of the hidden units
        Wij : np.array(), dim=(N,M)
            Weights between visible and hidden units
        S_kk : np.array(), dim=(k,k)
            Regularized covariance matrix.
            k = N + M + N*M (# of network weights)
        Fk : np.array(), dim=k
            Forces array.
            k = N + M + N*M (# of network weights)
        g : float
            Learning rate

        Returns
        -------
        a : np.array(), dim=N
            Updated nbases of the visible units
        b : np.array(), dim=M
            Updated biases of the hidden units
        w : np.array(), dim=(N,M)
            Updated weights between visible and hidden units
    """
    S_inv = np.linalg.inv(S_kk)
    weights = np.concatenate((Ai, Bj, Wij.flatten()), axis=None)
    weights = weights - g*np.matmul(S_inv, Fk)
    N, M = len(Ai), len(Bj)
    a = weights[:N]
    b = weights[N:N+M]
    w = np.reshape(weights[N+M:], (N,M))

    return a, b, w

# %% covariance matrix
def Skk(Opk, it):
    """
    Returns the covariance matrix given in (A4).
    Explicit regularization is applied as described in Appendix A.

        Parameters
        ----------
        Opk : np.array(), dim=(p,k)
            Matrix with the derivatives of the RBM wave function
            wrt to the network parameters.
            Each row represents a different realization.
            p = # of different realizations
            k = N + M + N*M (# of network weights)
        it : int
            Weights update iteration. It is needed to apply the regularization.

        Return
        ------
        S_kk : np.array(), dim=(k,k)
            Regularized covariance matrix.
    """
    p, k = Opk.shape
    S_kk = np.zeros((k,k), dtype=complex)
    # < Ok* Ok' >
    for Ok_vec in Opk:
        Ok_star = np.conjugate(Ok_vec)
        for i in range(k):
            for j in range(k):
                S_kk[i,j] += Ok_star[i]*Ok_vec[j]
    S_kk /= p

    # < Ok >
    Ok_mean = np.mean(Opk, axis=0)

    # < Ok* Ok' > - < Ok* > < Ok' >
    lp = max(100*0.9**it,10**(-4))
    for i,ok_star in enumerate(np.conjugate(Ok_mean)):
        for j,ok in enumerate(Ok_mean):
            S_kk[i,j] -= ok_star*ok
            if i == j: S_kk[i,j] += lp

    return S_kk

# %% forces
def Forces(Opk, Eloc):
    """
    Returns the forces as in (A5).

        Parameters
        ----------
        Opk : np.array(), dim=(p,k)
            Matrix with the derivatives of the RBM wave function
            wrt to the network parameters.
            Each row represents a different realization.
            p = # of different realizations
            k = N + M + N*M (# of network weights)
        Eloc : np.array(), dim=p
            Array with the local energies of the MC spin configurations

        Return
        ------
        Fk : np.array(), dim=k
            Forces array
    """
    p, k = Opk.shape
    Opk_star = np.conjugate(Opk)
    Fk = []
    # < Eloc Ok* >
    for i in range(k):
        Fk.append(np.dot(Eloc, Opk_star[:,i]))
    Fk = np.array(Fk) / p
    Fk -= np.mean(Eloc)*np.mean(Opk_star, axis=0)

    return Fk

# %% local energy
def LocalEnergy(config, H, Ai, Bj, Wij):
    """
    Returns the array with local energy for each spin
    configuration sampled from the Metropolis.

        Parameters
        ----------
        config : np.array(), dim=(p,N)
            Matrix in which each row corresponds to a configuration
            sapled by Metropolis.
            p = # of configurations
            N = # number of spins (eg. visible units)
        H : np.array(), dim=(2**N, 2**N)
            Matrix describing the hamiltonian of the system
        Ai : np.array(), dim=N
            Biases of visible units
        Bj : np.array(), dim=M
            Biases of hidden units
        Wij :np.array(), dim=(N,M)
            Weights between visible and hidden units

        Return
        ------
        Eloc : np.array(), dim=p
            Array with local energy values for each configuration
    """

    Eloc = []
    N = len(Ai)
    for S1 in config:
        idx = idx_from_config(S1)
        tmp = []
        for j,Hij in enumerate(H[idx,:]):
            if Hij != 0:
                S2 = config_from_idx(j, N)
                tmp.append(Hij*np.exp(logPsiDiff(S2, S1, Ai, Bj, Wij)))
        Eloc.append(np.sum(tmp))

    return np.array(Eloc)
