import networkx as nx
import EoN
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use("default")
from mpl_toolkits.mplot3d import axes3d
from matplotlib import cm
import seaborn as sns

def Net_generation(txt_path="network_rocha-etal.csv"):
    """
    Output: network,numero di nodi presenti
    """
    female, male, time = np.loadtxt(txt_path, unpack=True, comments="#", dtype=np.int16)
    dim = max(female.max(), male.max())+1
    adj = np.zeros((dim,dim))
    for i,j in zip(female,male):
        adj[i,j] = 1
        adj[j,i] = 1
    net = nx.from_numpy_array(adj)
    number_of_nodes= net.number_of_nodes()
    return(net,number_of_nodes)

def S_R_inf_surfaces(mu,beta,time,S,I,R,number_of_nodes):
    """
    Output: R/S infinito: sono i valori di suscettibili e recovered che si ottengono per ogni combinazione di mu e beta
    """
    S_inf=[]
    R_inf=[]
    Y=mu.ravel()
    X=beta.ravel()

    for i in range(len(S)):
        S_inf += [S[i][-1]]
        R_inf += [R[i][-1]]

    S_inf=np.array(S_inf)
    R_inf=np.array(R_inf)
    fig = plt.figure(figsize=(15,8))
    ax = fig.gca(projection='3d')
    plot = ax.plot_trisurf(X.tolist(),Y.tolist(),np.divide(S_inf,number_of_nodes),color="white",cmap=cm.inferno, vmin=0, vmax=1)
    #ax.set_title(r"Fraction of $S_\infty$ as function of $\beta$ and $\mu$",fontsize=16)
    ax.set_xlabel("$\\beta$ $[day^{-1}]$")
    ax.set_ylabel("$\\mu $ $[day^{-1}]$")
    ax.set_zlabel("$s _\\infty$")
    fig.colorbar(plot, shrink=0.5)
    plt.savefig("./Figure/test/S_inf_surface.pdf")
    plt.show()
    return S_inf, R_inf

def beta_mu_heatmap(beta, mu, x, N, fname=None):
    """
    Heatmap of x_inf as function of beta and mu

    Parameters
    ----------
    beta  : np.array()
        list of values of beta
    mu    : np.array()
        list of values of mu
    x     : np.array(dim=2)
        list with lists of the fraction of people in class x for each combination of beta/mu
    fname : str
        if provided, the heatmap is saved in a file

    Returns
    -------
    """
    x_inf = [float(elem[-1])/N for elem in x]
    beta  = beta.round(decimals=4)
    mu    = mu.round(decimals=4)
    df    = pd.DataFrame(index=np.unique(beta), columns=np.unique(mu))
    for idx in range(len(x_inf)):
        df.at[beta[idx], mu[idx]] = x_inf[idx]
    df = df.astype("float64")
    f = plt.figure()
    plt.tick_params(labelsize=10)
    if np.all(np.array(x_inf)>=0):
        vmax = 1
        vmin = 0
        center = None
        label = "$s_\\infty$"
    else:
        vmax = None
        vmin = None
        center = 0
        label = "$(s_\\infty - s_\\infty^{RRM}) / s_\\infty$"
    sns.heatmap(df, vmin=vmin, vmax=vmax, center=center, linewidths=.05, cbar_kws={"label":label, "aspect":40, "format":"%g"})
    cbar = f.axes[-1]
    cbar.tick_params(labelsize=10)
    plt.xlabel("$\\mu$ [day$^{-1}$]")
    plt.ylabel("$\\beta$ [day$^{-1}$]")
    if fname is not None: plt.savefig(fname)
    plt.show()

    return

def I_max_surfaces(mu,beta,time,S,I,R,number_of_nodes):
    """
    Output: I/t infinito: sono i valori di suscettibili e recovered che si ottengono per ogni combinazione di mu e beta
    """
    higher_infection_time=[]
    I_max=[]
    Y=mu.ravel()
    X=beta.ravel()
    for i in range(len(time)):
        index=np.argmax(I[i])
        higher_infection_time += [time[i][index]]
        I_max += [I[i][index]]
    I_max=np.array(I_max)
    higher_infection_time=np.array(higher_infection_time)

    fig = plt.figure(figsize=(15,8))
    ax = fig.gca(projection='3d')
    plot = ax.plot_trisurf(X.tolist(),Y.tolist(),np.divide(np.array(I_max),number_of_nodes),color="white",cmap=cm.inferno, vmin=0, vmax=1)
    ax.set_title("Fraction of max infected as function of $\\beta$ and $\\mu$",fontsize=16)
    ax.set_xlabel("$\\beta$")
    ax.set_ylabel("$\\mu$")
    ax.set_zlabel("$I_\\max$")
    fig.colorbar(plot, shrink=0.5)
    plt.show()

    fig = plt.figure(figsize=(15,8))
    ax = fig.gca(projection='3d')
    plot = ax.plot_trisurf(X.tolist(),Y.tolist(),higher_infection_time,color="white",cmap=cm.inferno)
    ax.set_title("Time of max infection as function of $\\beta$ and $\\mu$",fontsize=16)
    ax.set_xlabel("$\\beta$")
    ax.set_ylabel("$\\mu$")
    ax.set_zlabel("$t$")
    fig.colorbar(plot, shrink=0.5)
    plt.show()

    return I_max,higher_infection_time

def tagli(mu,beta,cosa_tagliare,number_of_nodes,values,title,y_label):
    """
    Input:  - mu e beta sono quelli che carico dal file .npy
            - cosa tagliare: è una lista di cosa voglio ci sia in y del mio grafico come ad esempio S_inf o I_max
            - number of node me lo da fuori Net_generation
            - values sono i tagli in cui voglio fare i grafici in funzione di beta e di mu
            - title: "franction of MY_DATA"
            - y_label: "MY_DATA"
    Output:
            - Grafico_funzione_di_mu: sono le x e le y del grafico dell'input in fuozione di mu per i valori dati
    """
    Y=mu.ravel()
    X=beta.ravel()
    Grafico_funzione_di_mu=[]
    Grafico_funzione_di_beta=[]

    plt.figure(figsize=(8,5))
    for v in values:
        sel = X == v
        Grafico_funzione_di_mu.append([Y[sel],np.divide(cosa_tagliare[sel], number_of_nodes)])
        plt.plot(Y[sel], np.divide(cosa_tagliare[sel], number_of_nodes),label="$\\beta$ = {:2f}".format(v))
    Tit_mu  = title + " as function of $\\mu$"
    plt.title(Tit_mu)
    plt.xlabel("$\\mu$")
    plt.legend()
    plt.show()

    plt.figure(figsize=(8,5))
    for v in values:
        sel = Y == v
        Grafico_funzione_di_beta.append([X[sel], np.divide(cosa_tagliare[sel], number_of_nodes)])
        plt.plot(X[sel], np.divide(cosa_tagliare[sel], number_of_nodes), label="$\\mu$"+" = {:2f}".format(v))
    Tit_beta= title + " as function of $\\beta$"
    plt.title(Tit_beta)
    plt.xlabel("$\\beta$")
    plt.legend()
    plt.show()

    return(Grafico_funzione_di_mu,Grafico_funzione_di_beta)

def temporal_network_list(edge1, edge2, time, aggregation=1, randomize=None, seed=None):
    '''
    Return a list of nx.Graph from a temporal list of links.

    Parameters
    ----------
    edge1 : np.array()
        list with the first edge of each link
    edge2 : np.array()
        list with the second edge of each link
    time  : np.array()
        list with the time in which each link is active
    aggregation : int
        time window used to create an aggregate network
    randomize : str
        randomize reference model:
        - None    : keep time sequence and links
        - "time"  : keep the links but reshuffle time sequence
        - "nodes" : keep the time sequence but reshuffle nodes lists
    seed : int
        seed use to shuffle

    Returns
    -------
    net_list : list(nx.Graph)
        list with the sequence of networks
    aggregation : int
        time window used to create an aggregate network
    '''
    net_list = []
    T = np.unique(time)
    Tmax = T[-1]
    # set the seed
    if seed is not None: np.random.seed(seed)
    # apply randomization if required
    if randomize == "time":
        np.random.shuffle(time)
    elif randomize == "nodes":
        np.random.shuffle(edge1)
        np.random.shuffle(edge2)
    else:
        pass
    # get network list
    for t in range(Tmax // aggregation + 1):
        time_window = np.logical_and(time>=t*aggregation, time<(t+1)*aggregation)
        edges = np.stack((edge1[time_window], edge2[time_window]), axis=-1)
        net_list.append(nx.from_edgelist(edges))

    return net_list, aggregation

def SIR_temporal(net_list, time=None, tmax=np.inf, beta=.5, mu=.5, p_infected=0, p_removed=0, rho=.05, t_initial_seed=1, exclude_fraction=0):
    '''
    Simulation of SIR model over a temporal network

    Parameters
    ----------
    net_list : list(nx.Graph())
        list of network graph made by networkx package
    time : int
        time window for each net in net_list
    tmax : int
        maximum time for the simulation
    beta : float
        probability of transmitting the disease per contact
    mu   : float
        probability of recovering from the disease
    p_infected : float
        probability that a node is infected when it enters in the network
    p_removed  : float
        probability that a node is recovered when it enters in the network
    rho   : float
        fraction of infected nodes (with respect to the total nodes in the network)
    t_initial_seed   : int
        initial infected are chosen in the first t_initial_seed snapshots
    exclude_fraction : float
        after t=len(net_list)*time, exclude the first exclude_fraction*len(net_list) and start from the next one

    Returns
    -------
    S : list(int)
        number of susceptible per time step
    I : list(int)
        number of infected per time step
    R : list(int)
        number of recovered per time step
    T : list(int)
        time
    '''

    # get list of all nodes in the network during the entire epidemic and create a dictionary where each node is associated to "S" compartment
    nodes = set()
    nodes_t = []
    for net in net_list:
        nodes_t.append([n for n in net])
        nodes.update([n for n in net])
    status = {n : "S" for n in nodes}
    # choose initial infected
    nodes_t_initial_seed = set([i for nn in nodes_t[:t_initial_seed] for i in nn])
    max_infects = int(np.amin([len(nodes)*rho, len(nodes_t_initial_seed)]))
    first_infected = np.random.choice(list(nodes_t_initial_seed), size=max_infects, replace=False)
    for n in first_infected: status[n] = "I"
    # list to store number of S,I,R
    S, I, R, T = [], [], [], []

    periodic_start = int(len(net_list)*exclude_fraction)

    # clear node set
    nodes.clear()
    # spreading simulation
    for t in range(tmax):
        # apply periodic boundary condition with a lag
        if t == len(net_list)*time:
            net_list = net_list[periodic_start:]
            nodes_t  = nodes_t[periodic_start:]
        snap_time = (t%(len(net_list)*time)) // time
        # get the network at the specific time
        net = net_list[snap_time]
        #print("t = {} | snap = {} | list size = {} | net size = {}".format(t, snap_time, len(net_list), len(list(net.nodes))))
        # if a node enters in the network for the first time, it has a probability to be infected
        #new_nodes = [n for n in nodes_t[snap_time] if n not in nodes]
        new_nodes = [n for n in nodes_t[snap_time]]
        for n in new_nodes:
            if np.random.rand() < p_infected : status[n] = "I"
            if np.random.rand() < p_removed  : status[n] = "Removed"
        # store new infected
        new_infected = set()
        # cycle over edges
        for e in net.edges():
            nodeA, nodeB = e
            statusA, statusB = status[nodeA], status[nodeB]
            # if there is a contact between "I" and "S" add the "S" node to new_infected with probability beta
            if (statusA=="I" and statusB=="S") or (statusA=="S" and statusB=="I"):
                if   statusA == "I" and np.random.rand() < beta:
                    new_infected.add(nodeB)
                elif statusB == "I" and np.random.rand() < beta:
                    new_infected.add(nodeA)
        # recover "I" nodes with probability mu
        for n, s in status.items():
            if s=="I" and np.random.rand() < mu:
                status[n] = "R"
        # update new infected
        for n in new_infected:
            status[n] = "I"
        # count and save number of "S", "I", "R"
        n_S, n_I, n_R = 0, 0, 0
        for s in status.values():
            if   s=="S" : n_S += 1
            elif s=="I" : n_I += 1
            elif s=="R" : n_R += 1
        S.append(n_S)
        I.append(n_I)
        R.append(n_R)
        T.append(t)

        # if there are no more I, exit
        if n_I == 0: break

    return S, I, R, T

def SIS_temporal(net_list, time=None, tmax=np.inf, beta=.5, mu=.5, p_infected=0, p_removed=0, rho=.05, t_initial_seed=1, exclude_fraction=0):
    '''
    Simulation of SIR model over a temporal network

    Parameters
    ----------
    net_list : list(nx.Graph())
        list of network graph made by networkx package
    time : int
        time window for each net in net_list
    tmax : int
        maximum time for the simulation
    beta : float
        probability of transmitting the disease per contact
    mu   : float
        probability of recovering from the disease
    p_infected : float
        probability that a node is infected when it enters in the network
    p_removed  : float
        probability that a node is removed when it enters in the network
    rho   : float
        fraction of infected nodes (with respect to the total nodes in the network)
    t_initial_seed   : int
        initial infected are chosen in the first t_initial_seed snapshots
    exclude_fraction : float
        after t=len(net_list)*time, exclude the first exclude_fraction*len(net_list) and start from the next one

    Returns
    -------
    S : list(int)
        number of susceptible per time step
    I : list(int)
        number of infected per time step
    T : list(int)
        time
    '''

    # if the time list is absent or has different length with respect to the network sequence, it defaults to range(len(net_list))
    #if time == None or len(net_list) != len(time):
    #    time = np.arange(len(net_list))
    # get list of all nodes in the network during the entire epidemic and create a dictionary where each node is associated to "S" compartment
    nodes = set()
    nodes_t = []
    for net in net_list:
        nodes_t.append([n for n in net])
        nodes.update([n for n in net])
    #nodes  = np.unique(nodes_t)
    status = {n : "S" for n in nodes}
    # choose initial infected
    nodes_t_initial_seed = set([i for nn in nodes_t[:t_initial_seed] for i in nn])
    max_infects = int(np.amin([len(nodes)*rho, len(nodes_t_initial_seed)]))
    first_infected = np.random.choice(list(nodes_t_initial_seed), size=max_infects, replace=False)
    for n in first_infected: status[n] = "I"
    # list to store number of S,I,R
    S, I, T = [], [], []

    periodic_start = int(len(net_list)*exclude_fraction)

    # clear node setnp.array([Tr
    nodes.clear()
    # spreading simulation
    for t in range(tmax):
        # apply periodic boundary condition with a lag
        if t == len(net_list)*time:
            net_list = net_list[periodic_start:]
            nodes_t  = nodes_t[periodic_start:]
        snap_time = (t%(len(net_list)*time)) // time
        # get the network at the specific time
        net = net_list[snap_time]
        #print("t = {} | snap = {} | list size = {} | net size = {}".format(t, snap_time, len(net_list), len(list(net.nodes))))
        # if a node enters in the network for the first time, it has a probability to be infected
        #new_nodes = [n for n in nodes_t[snap_time] if n not in nodes]
        new_nodes = [n for n in nodes_t[snap_time]]
        for n in new_nodes:
            if np.random.rand() < p_infected: status[n] = "I"
            if np.random.rand() < p_removed : status[n] = "Removed"
        # store new infected
        new_infected = set()
        # cycle over edges
        for e in net.edges():
            nodeA, nodeB = e
            statusA, statusB = status[nodeA], status[nodeB]
            # if there is a contact between "I" and "S" add the "S" node to new_infected with probability beta
            if (statusA=="I" and statusB=="S") or (statusA=="S" and statusB=="I"):
                if   statusA == "I" and np.random.rand() < beta:
                    new_infected.add(nodeB)
                elif statusB == "I" and np.random.rand() < beta:
                    new_infected.add(nodeA)
        # recover "I" nodes with probability mu
        for n, s in status.items():
            if s=="I" and np.random.rand() < mu:
                status[n] = "S"
        # update new infected
        for n in new_infected:
            status[n] = "I"
        # count and save number of "S", "I"
        n_S, n_I= 0, 0
        for s in status.values():
            if   s=="S" : n_S += 1
            elif s=="I" : n_I += 1
        S.append(n_S)
        I.append(n_I)
        T.append(t)

        # if there are no more I, exit
        if n_I == 0: break

    return S, I, T