import numpy as np

class Environment:

    state = []
    goal = []
    boundary = []
    action_map = {
        0: [0, 0],
        1: [0, 1],
        2: [0, -1],
        3: [1, 0],
        4: [-1, 0],
    }
    
    wall=[[0,3],[1,3],[2,3],[3,3],[3,4],[6,0],[7,0],[6,1],[7,1],[6,2],[7,2],[6,7],[6,8],[6,9]]
    palude=[[0,7],[1,7],[2,7],[6,4],[6,5],[6,6],[7,4],[8,4]]
    
    def __init__(self, x, y, initial, goal):
        self.boundary = np.asarray([x, y])
        self.state = np.asarray(initial)
        self.goal = goal
    
    # the agent makes an action (0 is stay, 1 is up, 2 is down, 3 is right, 4 is left)
    def move(self, action):
        current_state=self.state
        reward = 0
        wall=[[1,3],[2,3],[3,3],[3,4],[6,1],[7,1],[6,2],[7,2],[6,7],[6,8]]
        palude=[[1,7],[2,7],[6,4],[6,5],[6,6],[7,4],[8,4]]
        movement = self.action_map[action]
        
        entered=False
        if (action == 0 and (self.state == self.goal).all()):
            reward = 1
            entered= True
        next_state = self.state + np.asarray(movement)
        
        
        
        if int(next_state[0])<0 or int(next_state[0])>9 or int(next_state[1])<0 or int(next_state[1])>9:
            #print("entrato in bound")
            reward = -1
            self.state=current_state
            entered=True
        if [int(next_state[0]),int(next_state[1])] in wall:
            #print("entrato in wall")
            reward = -1
            self.state=current_state
            entered=True
        if [int(next_state[0]),int(next_state[1])] in palude:
            #print("entrato in palude")
            reward = -0.5
            self.state = next_state
            entered=True
            
        if entered==False:
            #print("entrato in controllo superato")
            self.state = next_state
        return [self.state, reward]

    # map action index to movement
    """def check_boundaries(self, state):
        out = len([num for num in state if num < 0])
        out += len([num for num in (self.boundary - np.asarray(state)) if num <= 0])
        return out > 0
    """

    
