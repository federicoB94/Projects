#%%
import scipy.io
import numpy as np
from sklearn.model_selection import train_test_split
import torch
import torch.nn as nn
import torch.optim as optim
import torch.nn.functional as F
import matplotlib.pyplot as plt
plt.style.use("default")

#%% load the network

class Net(nn.Module): 
    
    def __init__(self, Nh1=256):
        super(Net,self).__init__()
        
        self.fc1 = nn.Linear(in_features=784, out_features=Nh1)
        self.relu = torch.nn.ReLU()
        self.fc2 = nn.Linear(Nh1, 10)
        
        
        
    def forward(self, x, additional_out=False):
        
        x = self.relu(self.fc1(x))
        out = self.fc2(x) 
        return out

net = Net()
# Load the state dict previously saved
net_state_dict = torch.load('net_parameters.torch')
# Update the network parameters
net.load_state_dict(net_state_dict)

# %% load data

mat = scipy.io.loadmat('./MNIST.mat')
X = mat['input_images'] 
Y= mat['output_labels']


X_test = torch.Tensor(X).float().view(-1, X.shape[1])
Y_test = torch.LongTensor(Y).squeeze()

total=0
correct=0

tot_da_printare=3
count=0
for i in range(0,int(Y_test.shape[0])):
    prediction=net(X_test[i])
    values, indices = prediction.max(0)
    if (Y_test[i]==indices):
        correct +=1
        if (count <= tot_da_printare):
            #print("input")
            #pixel=np.array(X_test[i].reshape((28, 28)))
            #plt.imshow(np.flip(np.rot90(pixel,3),1), cmap='gray')
            #plt.show()
            #print("label: ", Y_test[i])
            #print("prediction: ", indices)
            count +=1

    total +=1

print("Accuracy: ",correct/total *100," %")


# %% print some receptive fields

fc1_w = net.fc1.weight.data.numpy()
fc2_w =net.fc2.weight.data.numpy()

#print("Receptive field of first layer")

plt.figure()
plt.imshow(fc1_w[90].reshape(28,28).T, cmap="gray")
#plt.savefig('./figure/re_1.png')
#plt.show()


#print("Receptive field of second layer")

plt.figure()
plt.imshow(fc2_w[8].reshape(16,16).T, cmap="gray")
#plt.savefig('./figure/re_2.png')
#plt.show()


# %%


