# -*- coding: utf-8 -*-

import json
import torch
import argparse
from torch import optim, nn
from network import Network
from dataset import encode_text, decode_text, create_one_hot_matrix
from pathlib import Path
import numpy as np


##############################
##############################
## PARAMETERS
##############################
parser = argparse.ArgumentParser(description='Generate a chapter starting from a given text')

parser.add_argument('--chapter_seed', type=str, default='the', help='Initial text of the chapter')
parser.add_argument('--length', type=int ,default=100, help='max lenght of the output')
parser.add_argument('--model_dir',   type=str, default='model', help='Network model directory')


##############################
##############################
##############################

if __name__ == '__main__':
    
    ### Parse input arguments
    args = parser.parse_args()
    lunghezza_max = int(args.length)
    
    #%% Load training parameters
    model_dir = Path(args.model_dir)
    print ('Loading model from: %s' % model_dir)
    training_args = json.load(open(model_dir / 'training_args.json'))
      
    #%% Load encoder and decoder dictionaries
    number_to_char = json.load(open(model_dir / 'number_to_char.json'))
    char_to_number = json.load(open(model_dir / 'char_to_number.json'))
        
    #%% Initialize network
    net = Network(input_size=training_args['alphabet_len'], 
                  hidden_units=training_args['hidden_units'], 
                  layers_num=training_args['layers_num'])
        
    #%% Load network trained parameters
    net.load_state_dict(torch.load(model_dir / 'net_params.pth', map_location='cpu'))
    net.eval() # Evaluation mode (e.g. disable dropout)
    
    #%% Find initial state of the RNN
    with torch.no_grad():
        # Encode seed
        seed_encoded = encode_text(char_to_number, args.chapter_seed)
        # One hot matrix
        seed_onehot = create_one_hot_matrix(seed_encoded, training_args['alphabet_len'])
        # To tensor
        seed_onehot = torch.tensor(seed_onehot).float()
        # Add batch axis
        seed_onehot = seed_onehot.unsqueeze(0)
        # Forward pass
        net_out, net_state = net(seed_onehot)
        # Get the most probable last output index
        #next_char_encoded = net_out[:, -1, :].argmax().item()
        #with softmax

        out = nn.functional.softmax(net_out[:, -1, :],dim=1) 
        _, top_ix = torch.topk(out, k=3)
        choices = top_ix.tolist()
        predicted = np.random.choice(choices[0])
        next_char_encoded = predicted.item()
        







        # Print the seed letters
        print(args.chapter_seed, end='', flush=True)
        next_char = number_to_char[str(next_char_encoded)]
        print(next_char, end='', flush=True)
        
    #%% Generate chapter
    new_line_count = 0
    tot_char_count = 0
    while True:
        with torch.no_grad(): # No need to track the gradients
            # The new network input is the one hot encoding of the last chosen letter
            net_input = create_one_hot_matrix([next_char_encoded], training_args['alphabet_len'])
            net_input = torch.tensor(net_input).float()
            net_input = net_input.unsqueeze(0)
            # Forward pass
            net_out, net_state = net(net_input, net_state)
            # Get the most probable letter index
            #next_char_encoded = net_out.argmax().item()

            out = nn.functional.softmax(net_out[:, -1, :],dim=1) 
            _, top_ix = torch.topk(out, k=3)
            choices = top_ix.tolist()
            predicted = np.random.choice(choices[0])
            next_char_encoded = predicted.item()
            # Decode the letter
            next_char = number_to_char[str(next_char_encoded)]
            print(next_char, end='', flush=True)
            # Count total letters
            tot_char_count += 1
            # Count new lines
            if next_char == '\n':
                new_line_count += 1
            # Break if 20 lines or 3000 letters
            if new_line_count == 20 or tot_char_count > 3000:
                break
            if tot_char_count >=lunghezza_max:
                break
        
        
        
        
        
        
        
        
        
        
        
        
