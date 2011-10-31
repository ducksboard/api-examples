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
Sends values to a custom image ducksboard widget.

Can grab a frame from a webcam using OpenCV, or capture the current
desktop using GTK.
"""


import base64
import cStringIO
import Image
import json
import urllib2
import sys


def capture_desktop():
    """
    Capture the current desktop using GTK bindings.
    """
    import gtk.gdk

    window = gtk.gdk.get_default_root_window()
    width, height = window.get_size()
    pb = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 8, width, height)
    pb = pb.get_from_drawable(
        window, window.get_colormap(), 0, 0, 0, 0, width, height)
    return Image.fromstring('RGB', (width, height), pb.get_pixels())


def capture_webcam():
    """
    Grab a frame from the first detected webcam using OpenCV.
    """
    import opencv
    from opencv import highgui

    camera = highgui.cvCreateCameraCapture(0)
    cv_img = highgui.cvQueryFrame(camera)
    img = opencv.adaptors.Ipl2PIL(cv_img)
    highgui.cvReleaseCapture(camera)
    return img


def img_to_base64(img):
    """
    Encode the resulting frame as png base64 after resizing it to
    320x240 px (a bigger image would be reduced by ducksboard widget
    anyway).
    """
    buffer = cStringIO.StringIO()
    img = img.resize((320, 240))
    img.save(buffer, format='PNG')
    return buffer.getvalue().encode('base64').replace('\n', '')


def send_to_ducksboard(endpoint, apikey, source, caption):
    """
    Send image to ducksboard custom image widget.
    """
    msg = {'value': {'source': 'data:image/png;base64,%s' % source,
                     'caption': caption}}
    request = urllib2.Request(endpoint)
    auth = base64.encodestring('%s:x' % apikey)
    auth = auth.replace('\n', '')
    request.add_header('Authorization', 'Basic %s' % auth)
    urllib2.urlopen(request, json.dumps(msg))


if __name__ == '__main__':
    if len(sys.argv) < 5:
        print 'Usage: %s desktop|webcam caption endpoint apikey' % sys.argv[0]
        sys.exit(0)

    action = sys.argv[1]
    caption = sys.argv[2]
    endpoint = sys.argv[3]
    apikey = sys.argv[4]

    grabber = globals().get('capture_%s' % action)
    img = grabber()
    b64_data = img_to_base64(img)

    send_to_ducksboard(endpoint, apikey, b64_data, caption)
