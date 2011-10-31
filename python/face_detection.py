#!/usr/bin/env python


# Copyright (C) 2011 Ducksboard
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.


"""
A Ducksboard API usage example involving webcams and face detection!

The following script aims at showcasing Ducksboard realtime update from
a Python program.

When ran from the command line, it should grab a webcam frame per second,
detect any existing face in the image, and send updates to Ducksboard
widgets as the number of detected faces changes.

It does so by using OpenCV Haar cascade algorithm and a cascade file
to be found in the same directory from which the script is invoked.

To avoid blocking any of the involved parts (webcam frames grabbing,
numerical/textual Ducksboard updates and image updates) each of these
actions runs in a separate thread. Not very sexy, but works.

The script has only been tested on Linux, but should work on other
platforms where OpenCV and its Python bindings are available. That
should include MacOSX (through MacPorts) and even Windows, probably.

Have fun! :)
"""


import base64
import cStringIO
import json
import Queue
import sys
import threading
import time
import urllib2

import opencv
from opencv import highgui


class DucksboardAPIClient(object):
    """
    Performs the HTTP POST to Ducksboard API.
    """

    def __init__(self, endpoint, apikey):
        """
        An endpoint and api key are defined on construction and used as
        request target and authentication parameter.
        """
        self.endpoint = endpoint
        self.apikey = apikey

    def send(self, value):
        """
        Given a value to send to ducksboard, builds the JSON encoded
        message and performs the request using the client api key as
        basic auth username (Ducksboard won't check the password).
        """
        msg = {'value': value}
        request = urllib2.Request(self.endpoint)
        auth = base64.encodestring('%s:x' % self.apikey)
        auth = auth.replace('\n', '')
        request.add_header('Authorization', 'Basic %s' % auth)
        urllib2.urlopen(request, json.dumps(msg))


class SendToDucksboardThread(threading.Thread):
    """
    Ducksboard HTTP requests are performed in their own thread to avoid
    blocking on network communication. Image sending is specially
    time consuming, and would block the whole app for a while.

    This is a generic class to be subclassed by each endpoint handler.
    """

    def __init__(self, queue, endpoint, apikey):
        """
        The sender must be created with and endpoint and api key which
        will be used to instantiate a Ducksboars client.
        The queue will be used to consume changes in faces detection.
        """
        threading.Thread.__init__(self)
        self.queue = queue
        self.running = True
        self.client = DucksboardAPIClient(endpoint, apikey)

    def run(self):
        """
        Consume from the queue until the application is stopped or a
        False object is pushed into the queue.
        """
        while self.running:
            item = self.queue.get()
            if item is False:
                return
            value = self.build_value(item)
            self.client.send(value)

    def build_value(self, item):
        """
        Each descendant can override this method to create the structure
        expected by its endpoint type. The default is to perform no
        transformations on the provided value.
        """
        return item


class SendCounterThread(SendToDucksboardThread):
    """
    Sends integer values to the given counter endpoint.
    """

    def build_value(self, item):
        """
        Ensure the value to be sent is an integer.
        """
        return int(item)


class SendTimelineThread(SendToDucksboardThread):
    """
    Send timeline items to the timeline endpoint.
    """

    image_path = 'https://app.ducksboard.com/static/img/timeline/%s.png'

    # 2 scenarios are available: more and less faces detected than the
    # last time. Each scenario is a dict representing a Ducksboard
    # timeline item structure.
    scenarios = {
        'more': {
            'title': 'More people joining!',
            'image': image_path % 'created',
            'content': '%i people have joined in the last seconds.'},
        'less': {
            'title': 'People are leaving...',
            'image': image_path % 'deleted',
            'content': '%i people have left just now!'},
    }

    def __init__(self, queue, endpoint, apikey):
        SendToDucksboardThread.__init__(self, queue, endpoint, apikey)
        self.last_value = 0

    def build_value(self, value):
        """
        Return a timeline item reflecting if more or less faces have been
        detected since the last change.
        """
        if value < self.last_value:
            scenario = dict(self.scenarios.get('less'))
        elif value >= self.last_value:
            scenario = dict(self.scenarios.get('more'))

        scenario['content'] = (scenario['content'] %
                               abs(self.last_value - value))
        self.last_value = value

        return scenario


class SendImageThread(SendToDucksboardThread):
    """
    Send an image to the image endpoint.

    Sending an image implies building a base64 encoded representation
    of the given image and then sending the resulting huge string to the
    API. The process being slow, it would block faster counter and timeline
    updates, being the main reason to use threads to send all of them.
    """

    def build_value(self, image):
        """
        A Ducksboard image representation needs an image value (url or
        base64 encoded image) and a caption.
        """
        return {
            'source': 'data:image/png;base64,%s' % self.image_to_base64(image),
            'caption': 'Green rectangles only on pretty people :)'}

    def image_to_base64(self, image):
        """
        Encode the given opencv frame as png base64 after resizing it to
        320x240 px (a bigger image would be reduced by ducksboard widget
        anyway).
        """
        buffer = cStringIO.StringIO()
        image = opencv.adaptors.Ipl2PIL(image)
        image = image.resize((320, 240))
        image.save(buffer, format='PNG')
        return buffer.getvalue().encode('base64').replace('\n', '')


class ImageGrabThread(threading.Thread):
    """
    Use OpenCV to grab frames from the webcam.

    We'll only perform face detection and resulting Ducksboard updates
    once a second, but we need to consume from the webcam at a much
    higher pace or the buffering will result in images delayed by
    many seconds. That's why the frame retrieval runs in a separate
    thread.
    """

    def __init__(self, visualize=False):
        threading.Thread.__init__(self)
        self.camera = highgui.cvCreateCameraCapture(-1)
        self.image = None
        self.visualize = visualize
        self.running = True

    def run(self):
        """
        Consume images from the webcam at 25fps.
        If visualize is True, show the result in the screen.
        """
        if self.visualize:
            highgui.cvNamedWindow('DucksboardFace')

        while self.running:
            self.image = highgui.cvQueryFrame(self.camera)
            if self.visualize:
                highgui.cvShowImage('DucksboardFace', self.image)
            highgui.cvWaitKey(1000 / 25)


class FaceDetector(object):
    """
    Face detection utility using OpenCV Haar cascade.

    The code has been taken from http://benosteen.wordpress.com/2010/03/03/
    face-recognition-much-easier-than-expected/ .

    The Haar cascade file used is provided by Naotoshi Seo:
    http://code.google.com/p/tutorial-haartraining/source/browse/
    #svn/trunk/data/haarcascades .
    """

    def detect(self, image):
        # image size is needed by underlying opencv lib to allocate memory
        image_size = opencv.cvGetSize(image)

        # the algorithm works with grayscale images
        grayscale = opencv.cvCreateImage(image_size, 8, 1)
        opencv.cvCvtColor(image, grayscale, opencv.CV_BGR2GRAY)

        # more underlying c lib memory allocation
        storage = opencv.cvCreateMemStorage(0)
        opencv.cvClearMemStorage(storage)

        # equalize histogram
        opencv.cvEqualizeHist(grayscale, grayscale)

        # detect faces using haar cascade, the used file is trained to
        # detect frontal faces
        cascade = opencv.cvLoadHaarClassifierCascade(
            'haarcascade_frontalface_alt.xml', opencv.cvSize(1, 1))
        faces = opencv.cvHaarDetectObjects(
            grayscale, cascade, storage, 1.2, 2,
            opencv.CV_HAAR_DO_CANNY_PRUNING, opencv.cvSize(100, 100))

        # draw rectangles around faces
        for face in faces:
            opencv.cvRectangle(
                image, opencv.cvPoint(
                    int(face.x), int(face.y)),
                    opencv.cvPoint(int(face.x + face.width),
                    int(face.y + face.height)), opencv.CV_RGB(127, 255, 0), 2)

        # return faces casted to list here, otherwise some obscure bug
        # in opencv will make it segfault if the casting happens later
        return image, list(faces)


def main(counter_endpoint, timeline_endpoint, image_endpoint,
         apikey, visualize):
    """
    Start all the threads and perform face detection once a second.
    If the number of detected faces has changed since the last time, send
    updates to the involved Ducksboard widgets (counter, timeline and image)
    by pushing the new values into the corresponding queues.
    """
    last_num_faces = 0

    detector = FaceDetector()

    grabber = ImageGrabThread(visualize)
    grabber.start()

    # counter and timelines are updates pretty fast, so allow a buffer of 5
    # of them to send to the server
    counter_queue = Queue.Queue(5)
    counter_sender = SendCounterThread(counter_queue, counter_endpoint, apikey)
    counter_sender.start()

    timeline_queue = Queue.Queue(5)
    timeline_sender = SendTimelineThread(
        timeline_queue, timeline_endpoint, apikey)
    timeline_sender.start()

    # images processing is slow, don't buffer more than a Ducksboard update
    image_queue = Queue.Queue(1)
    image_sender = SendImageThread(image_queue, image_endpoint, apikey)
    image_sender.start()

    try:
        while True:
            new_image = opencv.cvCloneMat(grabber.image)
            image, faces = detector.detect(new_image)
            num_faces = len(faces)
            if num_faces != last_num_faces:
                last_num_faces = num_faces
                for sender, value in ((counter_sender, num_faces),
                                      (timeline_sender, num_faces),
                                      (image_sender, image)):
                    try:
                        sender.queue.put(value)
                    except Queue.Full:
                        pass
            time.sleep(1)
    except KeyboardInterrupt:
        # stop everything cleanly on ctrl-c
        grabber.running = False
        grabber.join()

        for sender in (counter_sender, timeline_sender, image_sender):
            sender.running = False
            try:
                sender.queue.put(False)
            except Queue.Full:
                pass
            sender.join()


if __name__ == '__main__':
    if len(sys.argv) < 5:
        print ('Usage: %s counter_endpoint timeline_endpoint '
               'image_endpoint apikey [visualize]' % sys.argv[0])
        sys.exit(0)

    counter_endpoint = sys.argv[1]
    timeline_endpoint = sys.argv[2]
    image_endpoint = sys.argv[3]
    apikey = sys.argv[4]
    try:
        visualize = bool(sys.argv[5])
    except Exception:
        visualize = False

    main(counter_endpoint, timeline_endpoint, image_endpoint,
         apikey, visualize)
