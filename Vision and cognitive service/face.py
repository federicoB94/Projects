import os
import numpy as np
import cv2 as cv

from tensorflow.keras.models import load_model
from tensorflow.keras.applications.mobilenet_v2 import preprocess_input

from params import FACE_MODEL_PATH
from params import WIDTH, HEIGTH


def load_face_detector():
    # info: https://github.com/opencv/opencv/tree/4.0.0-beta/samples/dnn/face_detector
    # res10_300x300_ssd_iter_140000.caffemodel
    protox = os.path.join(FACE_MODEL_PATH, "deploy.prototxt")
    # face detector is trained over WIDER face dataset
    weights = os.path.join(
        FACE_MODEL_PATH, "res10_300x300_ssd_iter_140000_fp16.caffemodel")
    face_detector = cv.dnn.readNet(protox, weights)
    return face_detector


def extract_face(image, net, face_detector, confidence):

    h, w = image.shape[:2]
    # resize the image and perfrom mean subtraction foe each channel
    blob = cv.dnn.blobFromImage(
        image, 1.0, (300, 300), (104., 117., 123.))  # colors are BGR
    face_detector.setInput(blob)
    objects = face_detector.forward()

    faces, locations, predictions = [], [], []

    for i in range(0, objects.shape[2]):
        conf = objects[0, 0, i, 2]
        if conf < confidence:
            continue
        # extract face box borders
        box = objects[0, 0, i, 3:7]*np.array([w, h, w, h])
        l, t, r, b = box.astype("int")
        # ensure borders are inside the image
        l, r = max(0, l), min(w-1, r)
        b, t = max(0, b), min(h-1, t)
        # crop the face from the image with array slicing
        face = image[t:b, l:r]
        # change colors to RGB for net
        face = cv.cvtColor(face, cv.COLOR_BGR2RGB)
        face = cv.resize(face, (WIDTH, HEIGTH))
        face = preprocess_input(face)
        # add a dimension to match net input dimensions
        # (it is the batch size)
        # face = np.expand_dims(face, axis=0)
        faces.append(face)
        locations.append((l, t, r, b))

    if len(faces) > 0:
        # make prediction mask/no mask
        faces = np.array(faces)
        predictions = [float(p) for p in net.predict(faces)]

    return locations, predictions

    # object is a blob with a shape 1x1xNx7 where:
    #   - N is the number of detections
    #   - for every detection there is a vector of values  [batchId, classId, confidence, left, top, right, bottom]

    # return objects


def annotate(image, locations, predictions):
    for box, pred in zip(locations, predictions):
        l, t, r, b = box
        if pred < 0.5:
            label = "With Mask:"
            # box color in BGR
            color = (0, 255, 0)
            # get the accuracy as 1-pred
            pred = 1-pred
        else:
            label = "Without Mask:"
            # box color in BGR
            color = (0, 0, 255)
        # draw box an text
        cv.rectangle(image, (l, b), (r, t), color, 2)
        cv.putText(image, label, (l, b+15),
                   cv.FONT_HERSHEY_SIMPLEX, 0.6, color, thickness=2)
        cv.putText(image, "{:.2f} %".format(pred*100), (l, b+35),
                   cv.FONT_HERSHEY_SIMPLEX, 0.6, color, thickness=2)
    return image


def mask_recognition_image(model, image_path, confidence=0.75, save=False):
    net = load_model(os.path.join(model["name"], "net.h5"))
    face_detector = load_face_detector()
    im = cv.imread(image_path)
    locs, preds = extract_face(im, net, face_detector, confidence)
    im = annotate(im, locs, preds)
    if save:
        fname = image_path.split(
            ".")[0] + "_pred_{:03d}_{}.png".format(int(confidence*100), model["name"])
        cv.imwrite(fname, im)
    cv.imshow("Image", im)
    if cv.waitKey(0) == ord('q'):
        return


def mask_recognition_video(model, video=0, confidence=0.75, save=False):
    net = load_model(os.path.join(model["name"], "net.h5"))
    face_detector = load_face_detector()
    cap = cv.VideoCapture(video)
    if save:
        w = int(cap.get(cv.CAP_PROP_FRAME_WIDTH))
        h = int(cap.get(cv.CAP_PROP_FRAME_HEIGHT))
        fps = cap.get(cv.CAP_PROP_FPS)
        vname = "stream" if video == 0 else video.split(".")[0]
        fname = vname + \
            "_pred_{:03d}_{}.mp4".format(int(confidence*100), model["name"])
        fourcc = cv.VideoWriter_fourcc("M", "P", "4", "V")
        out = cv.VideoWriter(fname, fourcc, fps, (w, h))
    while cap.isOpened():
        ret, frame = cap.read()
        # if frame is read correctly ret is True
        if not ret:
            print("Can't receive frame (stream end?). Exiting ...")
            break
        locs, preds = extract_face(frame, net, face_detector, confidence)
        frame = annotate(frame, locs, preds)
        # gray = cv.cvtColor(frame, cv.COLOR_BGR2GRAY)
        # cv.imshow('frame', gray)
        if save:
            out.write(frame)
        cv.imshow("frame", frame)
        if cv.waitKey(1) == ord('q'):
            break
    # When everything done, release the capture
    cap.release()
    if save:
        out.release()
    cv.destroyAllWindows()
    return
