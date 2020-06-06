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
from torch.optim import Adam, SGD
from sklearn.metrics import accuracy_score


#%%
mat = scipy.io.loadmat('./MNIST.mat')
mat.keys()
X = mat['input_images'] 
Y= mat['output_labels']

"""shape check"""
print("shape X: ",np.shape(X))
print("shape Y: ",np.shape(Y))

"""Train-Test split"""
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.20,random_state=1994)

print("shape X_train: ",np.shape(X_train))
print("shape Y_train: ",np.shape(Y_train))
print("shape X_test: ",np.shape(X_test))
print("shape Y_test: ",np.shape(Y_test))

# Convert to tensor object
visual= torch.Tensor(X_train).float().view(-1, X_train.shape[1])
X_train = torch.Tensor(X_train).float().view(-1, X_train.shape[1])
Y_train = torch.LongTensor(Y_train).squeeze()
X_test = torch.Tensor(X_test).float().view(-1, X_test.shape[1])
Y_test = torch.LongTensor(Y_test).squeeze()

print("shape X_train: ",np.shape(X_train))
print("shape Y_train: ",np.shape(Y_train))
print("shape X_test: ",np.shape(X_test))
print("shape Y_test: ",np.shape(Y_test))

#%%
class Net(nn.Module): 
    
    def __init__(self, Nh1=256):
        super(Net,self).__init__()
        
        self.fc1 = nn.Linear(in_features=784, out_features=Nh1)
        self.relu = torch.nn.ReLU()
        #self.fc2 = nn.Linear(in_features=Nh1, out_features=Nh2)
        self.fc2 = nn.Linear(Nh1, 10)
        self.sigmoid = nn.Sigmoid()
        
        
    def forward(self, x, additional_out=False):
        
        x = self.relu(self.fc1(x))
        #x= self.relu(self.fc2(x))
        out = self.fc2(x) 
        return out

#%%
lr=[0.7]

accuratezza=[]
for l in lr:
    net = Net()
    loss_fn =nn.CrossEntropyLoss()
    optimizer = torch.optim.SGD(net.parameters(), lr = l)

    num_epochs= 5000

    net.train()
    train_loss_log = []
    test_loss_log = []
    for num_ep in range(num_epochs):

        optimizer.zero_grad()
        out = net(X_train)
        loss = loss_fn(out, Y_train)
        loss.backward()
        optimizer.step()

        train_loss_log.append(float(loss.data))

        net.eval()
        with torch.no_grad():
            out=net(X_test)
            test_loss=loss_fn(out,Y_test)
            test_loss_log.append(float(test_loss.data))

    

    # %%Valuto accuratezza
    total=0
    correct=0
    for i in range(0,int(Y_test.shape[0])):
        prediction=net(X_test[i])
        values, indices = prediction.max(0)
        if (Y_test[i]==indices):
            correct +=1
        total +=1
    


    #%% Plot losses
    plt.figure()
    plt.semilogy(train_loss_log, label='Train loss')
    plt.semilogy(test_loss_log, label='Test loss')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.grid()
    plt.legend()
    plt.tight_layout()
    plt.show()
    total=0
    correct=0
    for i in range(0,int(Y_test.shape[0])):
        prediction=net(X_test[i])
        values, indices = prediction.max(0)
        if (Y_test[i]==indices):
            correct +=1
        total +=1

    print(correct/total *100," %")
    accuratezza.append(correct/total *100)
    


#%% saving the network
net_state_dict = net.state_dict()
# Save the state dict to a file
torch.save(net_state_dict, 'net_parameters.torch')

# %% visualization

pixels = X_train[3458].reshape((28, 28))

# Plot
plt.imshow(pixels, cmap='gray')
plt.show()


# %%
plt.figure()
plt.plot(lr,accuratezza)
plt.title("Accuracy on the test set ")
plt.xlabel("Learning rate")
plt.ylabel("Accuracy")
plt.text(0.5, 93, "Best accuracy: "+str(np.max(accuratezza))+" %", fontsize=14,bbox=dict(facecolor='green', alpha=0.3))
plt.text(0.5, 92, "Best lr: "+str(lr[np.argmax(accuratezza)]), fontsize=14,bbox=dict(facecolor='green', alpha=0.3))
plt.savefig('lr_tuning.png')
plt.show()

# %%
