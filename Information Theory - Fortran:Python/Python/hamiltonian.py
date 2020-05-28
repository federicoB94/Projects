# %%
import numpy as np

# %% 1D Ising Hamiltonian
def Ising_1D(N,h):
    """

    Computes the Ising Hamiltonian for N 1/2 spin particles
    on 1D lattice.

        Parameters
        ----------
        N  : int
            Number of particles in the system
        h : float
            Strenght of single spin term
        Return
        ------
        H  : np.array(size=(N**2, N**2))
            Matrix representation of the Ising Hamiltonian
    """
    sigma_x = np.array([[0,1],[1,0]])
    sigma_z = np.kron(np.array([[1,0],[0,-1]]), np.array([[1,0],[0,-1]]))
    H = np.zeros((2**N,2**N))

    # self-interaction
    for i in range(1,N+1): #va da 1 a N
        if (i==1):
            H += np.kron(sigma_x, np.identity(2**(N-1)))
        elif(i!=1 and i!=N):
            H += np.kron(np.identity(2**(i-1)), np.kron(sigma_x, np.identity(2**(N-i))))
        elif(i==N):
            H += np.kron(np.identity(2**(N-1)),sigma_x)

    # interaction
    H_tmp = np.zeros((2**N,2**N))
    for i in range(1, N):
        if(i==1):
            H_tmp += np.kron(sigma_z, np.identity(2**(N-2)))
        elif(i!=1 and i!=N-1):
            tmp=np.kron(sigma_z,np.identity(2**(N-i-1))) #dx
            H_tmp += np.kron(np.identity(2**(i-1)), tmp) #sx
        elif(i==N-1):
            H_tmp += np.kron(np.identity(2**(N-2)), sigma_z)

    H = -(h*H + H_tmp)

    return H

def Ising_2D(N,h):
    #N è il numero di particelle della singola riga, in totale sono NxN
    I=np.array([[1,0],[0,1]])
    sigma_x=np.array([[0,1],[1,0]])
    sigma_z=np.array([[1,0],[0,-1]])
    Matrix=np.zeros((2**(N*N),2**(N*N)))

    # self-interaction
    for i in range(1,N*N+1): #va da 1 a N
        if (i==1):
            Matrix += np.kron(sigma_x,identity(N*N-1))
        elif(i!=1 and i!=N*N):
            Matrix += np.kron(identity(i-1),np.kron(sigma_x,identity(N*N-i)))
        elif(i==N*N):
            Matrix += np.kron(identity(N*N-1),sigma_x)

    # interaction con vicino a destra
    Matrix_2=np.zeros((2**(N*N),2**(N*N)))

    for i in range(1,N+1):
        for j in range(1,N+1):
            if(j==1):
                Matrix_2 += np.kron(identity((i-1)*N),np.kron(sigma_z,np.kron(sigma_z,identity((N-i)*N + N-2))))


            elif(j!=1 and j%N!=0):
                tmp=np.kron(sigma_z,np.kron(sigma_z,identity(N-j-1 + (N-i)*N)))
                Matrix_2 += np.kron(identity((i-1)*N+j-1),tmp)

    #interazione con quello sotto
    Matrix_3=np.zeros((2**(N*N),2**(N*N)))
    for i in range(1,N+1):
        for j in range(1,N+1):
            if (i!=N):
                dx = np.kron(sigma_z,identity((N-i-1)*N + N-j))
                inter= np.kron(sigma_z,identity(N-1))
                Matrix_3 += np.kron(identity((i-1)*N + j-1),np.kron(inter,dx))

    Matrix= -(h*Matrix + Matrix_2 +Matrix_3)

    return Matrix
