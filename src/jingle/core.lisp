;; Copyright (c) 2022 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :cl-playground.jingle.core
  (:use :cl)
  (:nicknames :playground.jingle.core)
  (:import-from :ningle)
  (:import-from :lack)
  (:import-from :clack)
  (:import-from :local-time)
  (:import-from :jonathan)
  (:export
   :*env*
   :start
   :stop
   :error-response
   :error-response-message
   :app
   :app-server
   :app-middlewares
   :make-app
   :set-response-header
   :set-response-status
   :with-json-response))
(in-package :cl-playground.jingle.core)

(defgeneric start (app)
  (:documentation "Starts the jingle application and serves requests"))

(defgeneric stop (app)
  (:documentation "Stops the jingle application and the underlying HTTP server"))

(defgeneric configure (app)
  (:documentation "Performs any steps needed to configure the
application, before starting it up"))

(defparameter *env* nil
  "*ENV* will be dynamically bound to the Lack environment. It can be
used to query the environment from within the HTTP handlers.")

(defmethod jonathan:%to-json ((timestamp local-time:timestamp))
  (jonathan:%to-json (local-time:timestamp-to-unix timestamp)))

(defclass error-response ()
  ((message
    :initarg :message
    :initform (error "Must specify error message")
    :reader error-response-message
    :documentation "The message to send out as part of the HTTP response"))
  (:documentation "An HTTP response representing an error"))

(defmethod jonathan:%to-json ((object error-response))
  (jonathan:with-object
    (jonathan:write-key-value "message" (error-response-message object))))

(defclass app (ningle:app)
  ((http-server
    :initarg :http-server
    :initform nil
    :accessor http-server
    :documentation "The underlying HTTP server of the app. Do not set this slot directly.")
   (http-server-kind
    :initarg :http-server-kind
    :initform :hunchentoot
    :accessor http-server-kind
    :documentation "The HTTP server to use, e.g. :hunchentoot, :woo, etc.")
   (middlewares
    :initarg :middlewares
    :initform nil
    :accessor middlewares
    :documentation "The list of middlewares for the app")
   (address
    :initarg :address
    :initform "127.0.0.1"
    :accessor address
    :documentation "The address on which the server will listen on")
   (port
    :initarg :port
    :initform 5000
    :accessor port
    :documentation "The port on which the server will listen to")
   (debug-mode
    :initarg :debug-mode
    :initform t
    :accessor debug-mode
    :documentation "If set to T, will start the app in debug mode")
   (silent
    :initarg :silent
    :initform nil
    :accessor silent
    :documentation "Do not output informational messages from Clack, if set to T")
   (use-threads
    :initarg :use-threads
    :initform t
    :accessor use-threads
    :documentation "Use threads, if set to T"))
  (:documentation "The jingle app"))

(defmethod lack.component:call ((app app) env)
  "Dynamically binds *ENV* to the Lack environment before it hits the
HTTP handlers.  This way the HTTP handlers can interact with the
surrounding environment exposed by Lack.

As of now, December 2022, a regular ningle handler does not have
access to the Lack environment, which prevents HTTP handlers from
using additional metadata present in the environment.

The Lack environment is something that is also used by middlewares, so
making that available to HTTP handlers allows our handlers to share
additional metadata, which may be pushed by middlewares.

See [1], [2] and [3] for more details.

[1]: https://dnaeon.github.io/common-lisp-web-dev-ningle-middleware/
[2]: https://github.com/fukamachi/ningle/issues/41
[3]: https://github.com/fukamachi/lack#the-environment"
  (let ((*env* env))
    (call-next-method)))

(defmethod configure ((app app))
  "Sets up the final application by applying all middlewares to our
jingle app"
  (with-accessors ((middlewares middlewares)) app
    (loop :with wrapped-app = (lack:builder app)
          :for middleware :in (reverse middlewares) :do
            (setf wrapped-app (lack:builder middleware wrapped-app))
          :finally (return wrapped-app))))

;; TODO: Restart cases
(defmethod start ((app app))
  "Configures the jingle app and starts serving HTTP requests"
  (with-accessors ((http-server http-server)
                   (http-server-kind http-server-kind)
                   (address address)
                   (port port)
                   (debug-mode debug-mode)
                   (silent silent)
                   (use-threads use-threads)) app
    (when http-server
      (error "Server is already started"))
    (let ((configured-app (configure app)))
      (setf http-server
            (clack:clackup configured-app
                           :server http-server-kind
                           :address address
                           :port port
                           :debug debug-mode
                           :silent silent
                           :use-threads use-threads)))))

;; TODO: Restart cases
(defmethod stop ((app app))
  "Stops the jingle application"
  (with-accessors ((http-server http-server)) app
    (unless http-server
      (error "Server is not started"))
    (clack:stop http-server)
    (setf http-server nil)))

(defun set-response-header (name value)
  "Sets the HTTP header with the given NAME to VALUE to be sent to the
client as part of the HTTP response. Internally ningle's response is
an instance of LACK.RESPONSE:RESPONSE. This function is meant to be
used from within handlers."
  (setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*)
                (list name value))))
