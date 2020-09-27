# %%
# import base libraries
import os
import matplotlib.pyplot as plt
import numpy as np
import pickle
import argparse
# import opencv
import cv2 as cv
# import tensorflow
import tensorflow as tf
from tensorflow.keras.utils import plot_model
# NN model
from params import MOBILE_NET_V2, CONV_NET


from network import NN_train
from face import mask_recognition_image, mask_recognition_video

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-t", "--train", action="store_true",
                       help="Train the Neural Network")
    group.add_argument("-i", "--image", type=str,
                       help="Prediction over an image")
    group.add_argument("-v", "--video", type=str,
                       help="Prediction over a video", nargs="?", default="0")
    group.add_argument("-p", "--plot", action="store_true",
                       help="Plot Model")
    parser.add_argument(
        "-m", "--model", type=str, choices=["MobNet", "ConvNet"], default="ConvNet")
    parser.add_argument("-c", "--confidence", type=float, default=0.75)
    parser.add_argument("-s", "--save", action="store_true",
                        help="Wheter to save the image or video")
    args = vars(parser.parse_args())
    # print(args)
    if args["model"] == "MobNet":
        model = MOBILE_NET_V2
    else:
        model = CONV_NET

    if args["train"]:
        NN_train(model)
    elif args["plot"]:
        net = model["network"]()
        plot_model(net, to_file=os.path.join(
            model["name"], "model.png"), show_shapes=True)
        print(net.summary())
    elif args["image"] is not None:
        mask_recognition_image(
            model, args["image"], args["confidence"], args["save"])
    else:
        video = 0 if args["video"] is None else args["video"]
        mask_recognition_video(model, video, args["confidence"], args["save"])

    #mask_recognition(MOBILE_NET_V2, IMAGE)

    # %%
