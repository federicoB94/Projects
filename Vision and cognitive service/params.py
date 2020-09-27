import os
# import keras modules
from tensorflow.keras.optimizers.schedules import ExponentialDecay
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.applications import mobilenet_v2
from tensorflow.keras.applications import MobileNetV2

import network
# %%
# Path to data files
DATA_PATH = "Face Mask Dataset"

TRAIN_DATA = os.path.join(DATA_PATH, "Train")
#VALIDATION_DATA = os.path.join(DATA_PATH, "Validation")
TEST_DATA = os.path.join(DATA_PATH, "Test")

FACE_MODEL_PATH = "face_detector"
IMAGE = "test.jpg"

# Image size for NN
HEIGTH, WIDTH = 224, 224

# Neural Network parameters
BATCH_SIZE = 128
INIT_LR = 1e-03
DECAY_RATE = 0.95
EPOCHES = 20

# NN Models
MOBILE_NET_V2 = {
    "name": "MobileNetV2",
    "network": network.build_MobNetV2
}

CONV_NET = {
    "name": "ConvNet",
    "network": network.build_Conv
}
