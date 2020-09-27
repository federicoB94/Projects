import os
import pickle
import matplotlib.pyplot as plt

import tensorflow as tf
from tensorflow.keras import Input
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras.applications.mobilenet_v2 import preprocess_input
from tensorflow.keras.applications import MobileNetV2
from tensorflow.keras.layers import AveragePooling2D, Flatten, Dense, Dropout, Conv2D, SpatialDropout2D, MaxPooling2D
from tensorflow.keras.models import Model, Sequential
from tensorflow.keras.optimizers.schedules import ExponentialDecay
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.callbacks import ModelCheckpoint

import params as p


def data_preprocessing():

    # Data augmentation for training
    datagen = ImageDataGenerator(
        rotation_range=20,
        width_shift_range=.2,
        height_shift_range=.2,
        horizontal_flip=True,
        preprocessing_function=preprocess_input,
    )
    # Data preprocessing for test set
    datagen_test = ImageDataGenerator(
        preprocessing_function=preprocess_input
    )

    # Generate train, validation and test sets
    train = datagen.flow_from_directory(
        p.TRAIN_DATA,
        target_size=(p.HEIGTH, p.WIDTH),
        batch_size=p.BATCH_SIZE,
        class_mode="binary"
    )
    test = datagen_test.flow_from_directory(
        p.TEST_DATA,
        target_size=(p.HEIGTH, p.WIDTH),
        batch_size=p.BATCH_SIZE,
        class_mode="binary"
    )
    # return train, test
    return train, test


def build_MobNetV2():
    print("Building MobileNetV2 ...")
    # load MobileNetV2 with Imagenet weigths
    base_model = MobileNetV2(
        input_shape=(p.HEIGTH, p.WIDTH, 3),
        include_top=False,
        weights="imagenet"
    )
    base_model.trainable = False
    # build head model on top
    head_model = base_model.output
    head_model = AveragePooling2D(pool_size=(7, 7))(head_model)
    head_model = Flatten()(head_model)
    head_model = Dense(128, activation="relu")(head_model)
    head_model = Dropout(.5)(head_model)
    head_model = Dense(1, activation="sigmoid")(head_model)
    # build complete NN
    net = Model(inputs=base_model.inputs, outputs=head_model)

    return net


def build_Conv():
    print("Building Conv Net ...")
    net = Sequential(
        [
            Input(shape=(p.HEIGTH, p.WIDTH, 3,)),
            Conv2D(filters=16, kernel_size=(3, 3),
                   padding="same", activation="relu"),
            SpatialDropout2D(0.3),
            MaxPooling2D(pool_size=(4, 4)),
            Conv2D(filters=32, kernel_size=(3, 3),
                   padding="same", activation="relu"),
            SpatialDropout2D(0.3),
            MaxPooling2D(pool_size=(4, 4)),
            Conv2D(filters=64, kernel_size=(3, 3),
                   padding="same", activation="relu"),
            AveragePooling2D(pool_size=(3, 3)),
            Flatten(),
            Dense(1024, activation="relu"),
            Dropout(0.3),
            Dense(512, activation="relu"),
            Dropout(0.3),
            Dense(1, activation="sigmoid")
        ]
    )

    return net


def training(model, train, test, checkpoints=True, plot=True):
    print("Building NN ...")
    # get NN
    net = model["network"]()
    # optimizer
    scheduler = ExponentialDecay(
        initial_learning_rate=p.INIT_LR,
        decay_steps=p.EPOCHES,
        decay_rate=p.DECAY_RATE
    )
    opt = Adam(learning_rate=scheduler)
    # compile the model
    net.compile(loss="binary_crossentropy",
                optimizer=opt, metrics=["accuracy"])
    # root dir
    root_dir = os.path.join(model["name"])
    os.makedirs(root_dir, exist_ok=True)
    # save checkpoints during training
    if checkpoints:
        folder = os.path.join(root_dir, "Checkpoints")
        os.makedirs(folder, exist_ok=True)
        checkpoints_callback = ModelCheckpoint(
            filepath=os.path.join(folder, "epoch_{epoch:02d}.h5")
        )
    print("Training NN ...")
    history = net.fit(
        train,
        epochs=p.EPOCHES,
        steps_per_epoch=train.n//train.batch_size,
        validation_data=test,
        validation_steps=test.n//test.batch_size,
        callbacks=[checkpoints_callback]
    )

    # save trained model and history
    print("Saving ...")
    net.save(os.path.join(root_dir, "net.h5"))
    with open(os.path.join(root_dir, "history.pkl"), "wb") as f:
        pickle.dump(history.history, f, protocol=pickle.HIGHEST_PROTOCOL)
    # plot history
    if plot is True:
        fig = plot_history(history, file=os.path.join(
            root_dir, "training.pdf"))
        return net, history, fig
    else:
        return net, history


def plot_history(history, file=None):
    print("Plotting NN train history ...")
    accuracy = history.history["accuracy"]
    loss = history.history["loss"]
    val_accuracy = history.history["val_accuracy"]
    val_loss = history.history["val_loss"]

    plt.grid(True)
    fig = plt.figure(figsize=(8, 5))
    plt.xlabel("Epoches")
    plt.plot(accuracy, "ro-", label="Train accuracy")
    plt.plot(loss, "bo-", label="Train loss")
    plt.plot(val_accuracy, "rD--", label="Validation accuracy")
    plt.plot(val_loss, "bD--", label="Validation loss")
    plt.legend()

    if file is not None:
        fig.savefig(file)

    return fig


def NN_train(model):
    if len(tf.config.list_physical_devices("GPU")) > 0:
        print("GPU found")
    else:
        print("Using CPU")
    print("Preprocessing data ...")
    train, test = data_preprocessing()
    print("Training ...")
    _, _, _ = training(model, train, test)
    return
