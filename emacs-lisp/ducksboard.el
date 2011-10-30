;;; ducksboard.el --- Ducksboard integration for Emacs

;; Copyright (C) 2011 Ducksboard

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


;;; Commentary:

;; This is a showcase package for Emacs Ducksboard integration.

;; Ducksboard (http://ducksboard.com/) is an online dashboard that
;; allows sending and visualising your own data. Included in this
;; package are hooks that will send Ducksboard events when you open,
;; save or close a file, so your team can know which files are you
;; editing or your obsessive compulsive boss can monitor if her
;; programmers are coding fast enough.

;; To enable it, put edit your .emacs file and put it it the following lines:
;; (require 'ducksboard)
;; (ducksboard-filehooks-enable)

;; Before you'll be able to use it, you have to customize the
;; ducksboard-api-key and ducksboard-file-operation-timeline-ids
;; variables, using M-x customize-group ducksboard.

;; Have fun!


(defgroup ducksboard nil
  "A simple showcase package for Emacs Ducksboard integration.

Before using it, customize the `ducksboard-api-key' and
`ducksboard-file-operation-timeline-ids' variables.

Features:
 * Send file events to Ducksboard, so the rest of your teams
   knows which file you have open
 * More to come!")

(defcustom ducksboard-url "https://push.ducksboard.com/"
  "The URL to use for sending events."
  :type       'string
  :group      'ducksboard)

(defcustom ducksboard-api-key ""
  "The API key to use for sending Ducksboard events."
  :type       'string
  :group      'ducksboard)

(defcustom ducksboard-file-operation-timeline-ids '()
  "A list of endpoint IDs where the events will be sent. If you
specify many, an event will be sent to each endpoint. For
instance, setting this to (list 232 354) will send events to
https://push.ducksboard.com/values/232/ and
https://push.ducksboard.com/values/354/"
  :type       '(repeat integer)

(defcustom ducksboard-interesting-file "."
  "A regular expression that gets matched against the file name
before an event gets sent. If the name does not match, the event
is not sent."
  :type	      'regexp
  :group      'ducksboard)

(defcustom ducksboard-file-name-transform 'identity
  "A function that gets passed the file name and should return a
transformed name, useful if you want to for example get rid of
part of the path."
  :type	      'function
  :group      'ducksboard)


(require 'url)
(require 'json)

(defun ducksboard-authorization (api-key)
  (cons "Authorization"
	(format "Basic %s" (base64-encode-string (format "%s:%s" api-key "x")))))

(defun ducksboard-send-internal (value-id api-key content)
  (let ((url (format "%s/values/%d/" ducksboard-url value-id))
	(url-request-method "POST")
	(url-request-data content)
	(url-request-extra-headers (list (ducksboard-authorization api-key))))
    (url-retrieve url (lambda (status)
			(bury-buffer (current-buffer))))))

(defun ducksboard-send-value (value-id value)
  (if (= 0 (length ducksboard-api-key))
      (message "Please customize the ducksboard-api-key variable")
    (let ((object (list :value value)))
      (ducksboard-send-internal value-id ducksboard-api-key (json-encode object)))))

(defun ducksboard-file-operation (operation)
  (let ((params (plist-get
		 '(open (image "created" verb "opened")
			edit (image "edited" verb "edited")
			close (image "deleted" verb "closed"))
		 operation))
	(file-name (buffer-file-name (current-buffer))))
    (cond ((not params) (message "unrecognized operation"))
	  ((not (stringp file-name)) nil)
	  ((not (string-match-p ducksboard-interesting-file file-name)) nil)
	  (t (let* ((transformed (apply ducksboard-file-name-transform (list file-name)))
		    (title (file-name-nondirectory file-name))
		    (image (format "https://app.ducksboard.com/static/img/timeline/%s.png"
				   (plist-get params 'image)))
		    (content (format "%s %s %s" (or (user-full-name) (user-login-name))
				     (plist-get params 'verb) transformed))
		    (value (list :title title :image image :content content)))
	       (mapc (lambda (value-id) (ducksboard-send-value value-id value))
		     ducksboard-file-operation-timeline-ids))))))


(defun ducksboard-find-file-hook ()
  (ducksboard-file-operation 'open))

(defun ducksboard-after-save-hook ()
  (ducksboard-file-operation 'edit))

(defun ducksboard-kill-buffer-hook ()
  (ducksboard-file-operation 'close))


(defun ducksboard-filehooks-enable ()
  (interactive)
  (add-hook 'find-file-hook 'ducksboard-find-file-hook)
  (add-hook 'after-save-hook 'ducksboard-after-save-hook)
  (add-hook 'kill-buffer-hook 'ducksboard-kill-buffer-hook))

(defun ducksboard-filehooks-disable ()
  (interactive)
  (remove-hook 'find-file-hook 'ducksboard-find-file-hook)
  (remove-hook 'after-save-hook 'ducksboard-after-save-hook)
  (remove-hook 'kill-buffer-hook 'ducksboard-kill-buffer-hook))

(provide 'ducksboard)
