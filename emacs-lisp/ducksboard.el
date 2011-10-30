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
;; allows sending and visualising your own data. The examples included
;; in this package can:

 ;; * send Ducksboard events when you open, save or close a file, so
 ;;   your team can know which files are you editing or your
 ;;   obsessive compulsive boss can monitor if her programmers are
 ;;   coding fast enough.

 ;; * send the number of keystrokes that you've done in your Emacs
 ;;   session, so you can see how your productivity falls on Friday
 ;;   evening.

;; To use the package, log to your Ducksboard and create a timeline
;; and a graph widget. The timeline will show file actions and the
;; graph will show the number of keystrokes per unit of time. Note
;; down your API key and the IDs where the data should be sent.

;; Then customize the ducksboard-api-key variable and the variables
;; that hold IDs of the data sources where your data will be
;; displayed.  Use M-x customize-group ducksboard for that.

;; To enable the package, edit your .emacs file and put the following
;; line in it
;; (require 'ducksboard)

;; To enable specific funcionality, evaluate the following, or
;; put in in your .emacs (by default everything is disabled)
;; (ducksboard-filehooks-enable)
;; (ducksboard-keystrokehooks-enable)

;; If you want to write your own, you can use the generic
;; ducksboard-send-value function that will send any value to
;; Ducksboard.

;; Have fun!


(defgroup ducksboard nil
  "A simple showcase package for Emacs Ducksboard integration.

Before using it, customize the `ducksboard-api-key' and widget ID
variables.

Features:
 * Send file events to Ducksboard, so the rest of your teams
   knows which file you have open
 * Send the number of keystrokes you made, so you can see which
   day of the week is most productive for you")

(defcustom ducksboard-url "https://push.ducksboard.com/"
  "The URL to use for sending events."
  :type 'string
  :group 'ducksboard)

(defcustom ducksboard-api-key ""
  "The API key to use for sending Ducksboard events."
  :type 'string
  :group 'ducksboard)


(defcustom ducksboard-file-operation-timeline-ids '()
  "A list of endpoint IDs where file operation events will be
sent. If you specify many, an event will be sent to each
endpoint. For instance, setting this to (list 232 354) will send
events to https://push.ducksboard.com/values/232/ and
https://push.ducksboard.com/values/354/"
  :type '(repeat integer)
  :group 'ducksboard)

(defcustom ducksboard-interesting-file "."
  "A regular expression that gets matched against the file name
before an event gets sent. If the name does not match, the event
is not sent."
  :type 'regexp
  :group 'ducksboard)

(defcustom ducksboard-file-name-transform 'identity
  "A function that gets passed the file name and should return a
transformed name, useful if you want to for example get rid of
part of the path."
  :type 'function
  :group 'ducksboard)


(defcustom ducksboard-keystrokes-graph-ids '()
  "A list of endpoint IDs where keystrokes counts will be sent. See
ducksboard-file-operation-timeline-ids for details"
  :type '(repeat integer)
  :group 'ducksboard)

(defcustom ducksboard-keystrokes-send-interval 10
  "An time interval in minutes between sending keystrokes counts."
  :type 'integer
  :group 'ducksboard)

(defcustom ducksboard-keystrokes-state-file "~/.ducksboard-keystrokes"
  "The file used to persist number of keystrokes between sessions."
  :type 'file
  :group 'ducksboard)


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


(defvar *ducksboard-keystrokes-count* -1
  "A keystrokes counter, where -1 means it needs to be loaded
  from the ondisk cache.")
(defvar *ducksboard-send-keystrokes-timer* nil
  "The timer that sends keystroke counts.")

(defun ducksboard-send-keystrokes ()
  (let ((value *ducksboard-keystrokes-count*))
    (mapc (lambda (value-id) (ducksboard-send-value value-id value))
	  ducksboard-keystrokes-graph-ids)))

(defun ducksboard-send-keystrokes-when-idle ()
  (run-with-idle-timer 2 nil 'ducksboard-send-keystrokes))

(defun ducksboard-post-command-hook ()
  (setq *ducksboard-keystrokes-count* (1+ *ducksboard-keystrokes-count*)))

(defun ducksboard-save-keystroke-count ()
  (when (/= *ducksboard-keystrokes-count* -1)
    (with-temp-buffer
      (insert (format "%d" *ducksboard-keystrokes-count*))
      (write-region (point-min) (point-max) ducksboard-keystrokes-state-file))))

(defun ducksboard-load-keystroke-count ()
  (when (file-readable-p ducksboard-keystrokes-state-file)
    (with-temp-buffer
      (insert-file-contents-literally ducksboard-keystrokes-state-file)
      (setq *ducksboard-keystrokes-count* (string-to-number (buffer-string))))))

(defun ducksboard-keystrokehooks-enable ()
  (interactive)
  (when (= *ducksboard-keystrokes-count* -1)
    (ducksboard-load-keystroke-count))
  (add-hook 'post-command-hook 'ducksboard-post-command-hook)
  (add-hook 'kill-emacs-hook 'ducksboard-save-keystroke-count)
  (unless *ducksboard-send-keystrokes-timer*
    (let ((minutes (* 60 ducksboard-keystrokes-send-interval)))
      (setq *ducksboard-send-keystrokes-timer*
	    (run-at-time minutes minutes 'ducksboard-send-keystrokes-when-idle)))))

(defun ducksboard-keystrokehooks-disable ()
  (interactive)
  (remove-hook 'post-command-hook 'ducksboard-post-command-hook)
  (when *ducksboard-send-keystrokes-timer*
    (cancel-timer *ducksboard-send-keystrokes-timer*)
    (setq *ducksboard-send-keystrokes-timer* nil)))

(provide 'ducksboard)
