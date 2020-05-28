import os
import torch
import matplotlib.pyplot as plt
import random
from torch import nn
import numpy as np
from torch.utils.data import DataLoader
from torchvision import transforms
from torchvision.datasets import MNIST
from tqdm import tqdm
import scipy.io


class Autoencoder(nn.Module):
    
    def __init__(self, encoded_space_dim):
        super().__init__()
        
        ### Encoder
        self.encoder_cnn = nn.Sequential(
            nn.Conv2d(1, 8, 3, stride=2, padding=1),
            nn.ReLU(True),
            nn.Conv2d(8, 16, 3, stride=2, padding=1),
            nn.ReLU(True),
            nn.Conv2d(16, 32, 3, stride=2, padding=0),
            nn.ReLU(True)
        )
        self.encoder_lin = nn.Sequential(
            nn.Linear(3 * 3 * 32, 64),
            nn.ReLU(True),
            nn.Linear(64, encoded_space_dim)
        )
        
        ### Decoder
        self.decoder_lin = nn.Sequential(
            nn.Linear(encoded_space_dim, 64),
            nn.ReLU(True),
            nn.Linear(64, 3 * 3 * 32),
            nn.ReLU(True)
        )
        self.decoder_conv = nn.Sequential(
            nn.ConvTranspose2d(32, 16, 3, stride=2, output_padding=0),
            nn.ReLU(True),
            nn.ConvTranspose2d(16, 8, 3, stride=2, padding=1, output_padding=1),
            nn.ReLU(True),
            nn.ConvTranspose2d(8, 1, 3, stride=2, padding=1, output_padding=1)
        )

    def forward(self, x):
        x = self.encode(x)
        x = self.decode(x)
        return x

    def encode(self, x):
        # Apply convolutions
        x = self.encoder_cnn(x)
        # Flatten
        x = x.view([x.size(0), -1])
        # Apply linear layers
        x = self.encoder_lin(x)
        return x
    
    def decode(self, x):
        # Apply linear layers
        x = self.decoder_lin(x)
        # Reshape
        x = x.view([-1, 32, 3, 3])
        # Apply transposed convolutions
        x = self.decoder_conv(x)
        x = torch.sigmoid(x)
        return x


device = torch.device("cpu")
net = Autoencoder(encoded_space_dim=6)
net.load_state_dict(torch.load('net_params_best_model.pth', map_location='cpu'))   



mat = scipy.io.loadmat('./MNIST.mat')
X = mat['input_images']
Y= mat['output_labels']
X= torch.Tensor(X).view(60000,1,28,28)

loss_fn = torch.nn.MSELoss()

def Evaluation(net,dataset,loss_fn):
    net.eval()
    loss_test=[]
    for image in dataset:
        img=torch.rot90(image[0],k=1)
        img=torch.flip(img,[0])
        img=img.reshape(1,1,28,28)
        
        with torch.no_grad():
            out = net(img)
            loss = loss_fn(out, img)
            loss_test.append(loss)
    return loss_test

loss_test= Evaluation(net,X,loss_fn)

print("MSE: ",np.mean(loss_test))