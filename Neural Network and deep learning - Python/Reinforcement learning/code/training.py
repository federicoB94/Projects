import dill
import numpy as np
import agent
import environment



episodes = 2000         # number of training episodes
episode_length = 50     # maximum episode length
x = 10                  # horizontal size of the box
y = 10                  # vertical size of the box
goal = [0, 4]           # objective point

softmax = True       # set to true to use Softmax policy
sarsa = True # set to true to use the Sarsa algorithm


wall=[[1,3],[2,3],[3,3],[3,4],[6,1],[7,1],[6,2],[7,2],[6,7],[6,8]]
palude=[[1,7],[2,7],[6,4],[6,5],[6,6],[7,4],[8,4]]
epsilon = np.linspace(0.8, 0.001,episodes)


def punto_iniziale(wall,x, y):
    pos = [[h,k] for h in range(x) for k in range(y) if [h,k] not in wall]
    choice = np.random.choice(len(pos))
    return np.array(pos[choice])

for discount in [0.9]:
    for lr in [0.5]:
        alpha=np.ones(episodes) * lr
        learner = agent.Agent((x * y), 5, discount, max_reward=1, softmax=softmax, sarsa=sarsa)
        A=True
        reward_sequence=[]
        tmp_results=[]
        for index in range(0,episodes):
            # start from a random state
            if index == episodes-1:
                initial=[9,9]
            else:
                initial = punto_iniziale(wall+palude,x,y)
            # initialize environment
            state = initial
            env = environment.Environment(x, y, state, goal)
            reward = 0
            # run episode
            passi=[]
            for step in range(0, episode_length):
                #print(state)
                #print(type(state))
                # find state index
                passi.append(state)
                state_index = int(state[0])* y + int(state[1])
                # choose an action
                action = learner.select_action(state_index, epsilon[index])
                # the agent moves in the environment
                result = env.move(action)
                # Q-learning update
                #print(result)
                next_index = int(result[0][0]) * y + int(result[0][1])
                learner.update(state_index, action, result[1], next_index, alpha[index], epsilon[index])
                # update state and reward
                reward += result[1]
                state = result[0]
            reward /= episode_length
            if (index%25==0):
                print("Index: ",index,"/2000"," reward: ",reward)

