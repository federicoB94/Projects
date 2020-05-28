# %%
import numpy as np

# %% log ratio between Psi
def logPsiDiff(Si2, Si1, Ai, Bj, Wij):
    """

    Returns the log of the ratio between the
    Network Quantum States described by spin configurations
    Si2 and Si1:
        log( Psi(Si2) / Psi(Si1) ) = log( Psi(Si2) ) - log( Psi(Si1) )

        Parameters
        ----------
        Si2 : np.array(), dim=N
            Final spin configuration (+- 1 array)
        Si1 : np.array(), dim=N
            Initial spin configuration (+- 1 array)
        Ai : np.array(), dim=N
            Biases of the visible units
        Bj : np.array(), dim=N
            Biases of the hidden units
        Wij : np.array(), dim=(N,M)
            Weights between visible and hidden units

        Return
        ------
        logPsi : complex
            log( Psi(Si2) ) - log( Psi(Si1) )
    """

    Tj2 = Bj + np.matmul(np.transpose(Wij), Si2)
    Tj1 = Bj + np.matmul(np.transpose(Wij), Si1)

    logPsiDiff = np.sum(Ai*(Si2-Si1)) + np.sum(np.log(np.cosh(Tj2))) - np.sum(np.log(np.cosh(Tj1)))

    return logPsiDiff

# %% int representation of spin config
def idx_from_config(config):
    """
    Returns the integer representation of a spin configuration.
    Each configuration can be represented in the 2**N space as
    a vector of 0, and one 1 in position 'idx'.

        Parameters
        ----------
        config : np.array(), dim=N
            Spin configuration (array with +-1)

        Returns
        -------
        idx : int
            Integer representation of config in the 2**N vector.
            It corresponds to the index of the element 1 in that array.
    """
    N = len(config)
    idx = 0
    for i, spin in enumerate(config):
        if spin == 1:
            idx += 2**(N-i-1)

    return idx

# %% spin config from int representation
def config_from_idx(idx, N):
    """
    Returns the spin configuration given the integer representation.
    It is the inverse of 'idx_from_config'

        Parameters
        ----------
        idx : int
            Integer representation of config in the 2**N vector.
            It corresponds to the index of the element 1 in that array.
        N : int
            Total number of spis in the configuration.

        Returns
        -------
        config : np.array(), dim=N
            Spin configuration (array with +-1)
    """
    config = -np.ones(N)
    binary = [int(d) for d in str(bin(idx))[2:]]
    binary.reverse()
    for i,b in enumerate(binary):
        if b == 1:
            config[i] = 1

    return np.flip(config)