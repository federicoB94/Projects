# %%
import numpy as np
from scipy.linalg import eigh_tridiagonal

# %%
def Lanczos(H,n_iter):
    dim_H = len(H)
    v1 = np.random.choice([1], size=dim_H)
    v1=v1/np.linalg.norm(v1)

    _w1_=np.matmul(H,v1)

    alpha_1 = np.dot(_w1_,v1)

    w1= _w1_-alpha_1*v1

    list_w=[w1]
    list_alpha=[alpha_1]
    list_v=[v1]
    list_b=[]

    for j in range (2,n_iter+1):
        beta_j = np.linalg.norm(list_w[-1])

        if beta_j==0:
            print("ERROR: beta=0")
            break
        w_j_last=list_w[-1]
        v_j=w_j_last/beta_j

        _w_j_ = np.matmul(H,v_j)
        alpha=np.dot(_w_j_,v_j)

        v_j_last=list_v[-1]

        w_j = _w_j_ - alpha* v_j - beta_j*v_j_last

        list_w.append(w_j)
        list_alpha.append(alpha)
        list_v.append(v_j)
        list_b.append(beta_j)

        EIG = eigh_tridiagonal(list_alpha, list_b, eigvals_only=True)

    return EIG