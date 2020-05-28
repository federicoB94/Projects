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





# %%
mat = scipy.io.loadmat('./MNIST.mat')

mat.keys()

X = mat['input_images'] #60'000 x 784 quindi dentro questa matriciona c'è una immagine per ogni riga e le colonne sono i vari pixel quindi per trattarle con una convolutional bisogna fare in seguito un reshape dei dati



Y= np.squeeze(mat['output_labels'])

"""shape check"""
print("shape X: ",np.shape(X))
print("shape Y: ",np.shape(Y))

"""Train-Test split"""
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.20,random_state=1994)

print("shape X_train: ",np.shape(X_train))
print("shape Y_train: ",np.shape(Y_train))
print("shape X_test: ",np.shape(X_test))
print("shape Y_test: ",np.shape(Y_test))


X_train = X_train.reshape(len(X_train),1,28,28)
X_train = torch.from_numpy(X_train)
X_test = X_test.reshape(len(X_test),1,28,28)
X_test = torch.from_numpy(X_test)
Y_train = Y_train.astype(int)
Y_train = torch.from_numpy(Y_train)
Y_test = Y_test.astype(int)
Y_test = torch.from_numpy(Y_test)

X_train.shape, Y_train.shape






#%%


class Net(nn.Module):   
    def __init__(self):
        super(Net, self).__init__()

        self.cnn_layers = nn.Sequential(
            # Defining a 2D convolution layer
            nn.Conv2d(1, 4, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(4),
            nn.ReLU(inplace=True),
            nn.MaxPool2d(kernel_size=2, stride=2),
            # Defining another 2D convolution layer
            nn.Conv2d(4, 4, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(4),
            nn.ReLU(inplace=True),
            nn.MaxPool2d(kernel_size=2, stride=2),
        )

        self.linear_layers = nn.Sequential(
            nn.Linear(4 * 7 * 7, 10)
        )

    # Defining the forward pass    
    def forward(self, x):
        x = self.cnn_layers(x)
        x = x.view(x.size(0), -1)
        x = self.linear_layers(x)
        return x

# %%

network = Net()
optimizer = Adam(network.parameters(), lr=0.07)
loss= nn.CrossEntropyLoss()



#%%
def train(epoch):
    network.train()
    tr_loss = 0
    # clearing the Gradients of the model parameters
    optimizer.zero_grad()
    
    
    output_train = network(X_train)
    output_val = network(X_test)

    # computing the training and validation loss
    loss_train = loss(output_train, Y_train)
    loss_val = loss(output_val, Y_test)
    train_losses.append(loss_train)
    val_losses.append(loss_val)

    # computing the updated weights of all the model parameters
    loss_train.backward()
    optimizer.step()
    tr_loss = loss_train.item()
    if epoch%2 == 0:
        # printing the validation loss
        print('Epoch : ',epoch+1, '\t', 'loss :', loss_val)

#%%
# defining the number of epochs
n_epochs = 100
# empty list to store training losses
train_losses = []
# empty list to store validation losses
val_losses = []
# training the model
for epoch in range(n_epochs):
    train(epoch)



# %%
plt.plot(train_losses, label='Training loss')
plt.plot(val_losses, label='Validation loss')
plt.legend()
plt.show()


# %%
with torch.no_grad():
    output = network(X_train)
    
softmax = torch.exp(output).cpu()
prob = list(softmax.numpy())
predictions = np.argmax(prob, axis=1)

# accuracy on training set
accuracy_score(Y_train, predictions)
