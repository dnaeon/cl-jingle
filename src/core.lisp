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
(defpackage :jingle.core
  (:use :cl)
  (:import-from
   :jingle.codes
   :get-status-code-or-lose)
  (:import-from :ningle)
  (:import-from :lack)
  (:import-from :lack.middleware.static)
  (:import-from :lack.middleware.mount)
  (:import-from
   :lack.request
   :request-env
   :request-method
   :request-script-name
   :request-path-info
   :request-server-name
   :request-server-port
   :request-server-protocol
   :request-uri
   :request-uri-scheme
   :request-remote-addr
   :request-remote-port
   :request-query-string
   :request-raw-body
   :request-content-length
   :request-content-type
   :request-headers
   :request-cookies
   :request-body-parameters
   :request-query-parameters
   :request-parameters
   :request-content
   :request-has-body-p
   :request-accept
   :request-accepts-p)
  (:import-from
   :lack.response
   :response-status
   :response-headers
   :response-body
   :response-set-cookies)
  (:import-from :lack.app.directory)
  (:import-from :clack)
  (:import-from :local-time)
  (:import-from :jonathan)
  (:export
   :*env*
   :start
   :stop
   :configure
   :add-middleware
   :static-path
   :serve-directory

   :error-response
   :error-response-message

   :app
   :make-app
   :http-server
   :http-server-kind
   :middlewares
   :address
   :port
   :debug-mode
   :silent-mode
   :use-threads

   :set-response-header
   :set-response-status
   :set-response-body
   :with-json-response
   :redirect

   ;; Re-exports from LACK.REQUEST
   :request-env
   :request-method
   :request-script-name
   :request-path-info
   :request-server-name
   :request-server-port
   :request-server-protocol
   :request-uri
   :request-uri-scheme
   :request-remote-addr
   :request-remote-port
   :request-query-string
   :request-raw-body
   :request-content-length
   :request-content-type
   :request-headers
   :request-cookies
   :request-body-parameters
   :request-query-parameters
   :request-parameters
   :request-content
   :request-has-body-p
   :request-accept
   :request-accepts-p

   ;; Re-exports from LACK.RESPONSE
   :response-status
   :response-headers
   :response-body
   :response-set-cookies))
(in-package :jingle.core)

(defgeneric start (app)
  (:documentation "Starts the jingle application and serves requests"))

(defgeneric stop (app)
  (:documentation "Stops the jingle application and the underlying HTTP server"))

(defgeneric configure (app)
  (:documentation "Performs any steps needed to configure the
application, before starting it up"))

(defgeneric static-path (app path root)
  (:documentation "Adds a static path to serve files from"))

(defgeneric add-middleware (app middleware)
  (:documentation "Adds a new middleware to the app"))

(defgeneric serve-directory (app path root)
  (:documentation "Serves the files from the given root directory"))

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
   (silent-mode
    :initarg :silent-mode
    :initform nil
    :accessor silent-mode
    :documentation "Do not output informational messages from Clack, if set to T")
   (use-threads
    :initarg :use-threads
    :initform t
    :accessor use-threads
    :documentation "Use threads, if set to T"))
  (:documentation "The jingle app"))

(defun make-app (&key (middlewares nil)
                   (server-kind :hunchentoot)
                   (address "127.0.0.1")
                   (port 5000)
                   (debug-mode t)
                   (silent-mode nil)
                   (use-threads t))
  "Creates a new jingle app"
  (make-instance 'app
                 :middlewares middlewares
                 :http-server-kind server-kind
                 :address address
                 :port port
                 :debug-mode debug-mode
                 :silent-mode silent-mode
                 :use-threads use-threads))

(defmethod lack.component:call ((app app) env)
  "Dynamically binds *ENV* to the Lack environment before it hits the
HTTP handlers.  This way the HTTP handlers can interact with the
surrounding environment exposed by Lack by using JINGLE.CORE:*ENV*"
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

(defmethod start ((app app))
  "Configures the jingle app and starts serving HTTP requests"
  (with-accessors ((http-server http-server)
                   (http-server-kind http-server-kind)
                   (address address)
                   (port port)
                   (debug-mode debug-mode)
                   (silent-mode silent-mode)
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
                           :silent silent-mode
                           :use-threads use-threads)))))

(defmethod stop ((app app))
  "Stops the jingle application"
  (with-accessors ((http-server http-server)) app
    (unless http-server
      (error "Server is not started"))
    (clack:stop http-server)
    (setf http-server nil)))

(defmethod add-middleware ((app app) middleware)
  (with-accessors ((middlewares middlewares)) app
    (setf middlewares (append middlewares (list middleware)))))

(defmethod static-path ((app app) path root)
  (add-middleware app
                  (lambda (app)
                    (funcall lack.middleware.static:*lack-middleware-static* app :path path :root root))))

(defmethod serve-directory ((app app) path root)
  (let ((dir-app (make-instance 'lack.app.directory:lack-app-directory :root root)))
    (add-middleware app
                    (lambda (app)
                      (funcall lack.middleware.mount:*lack-middleware-mount* app path dir-app)))))

(defun set-response-header (name value)
  "Sets the HTTP header with the given NAME to VALUE to be sent to the
client as part of the HTTP response. Internally ningle's response is
an instance of LACK.RESPONSE:RESPONSE. This function is meant to be
used from within handlers."
  (setf (response-headers ningle:*response*)
        (append (response-headers ningle:*response*)
                (list name value))))

(defun set-response-status (value)
  "Sets the status code for the HTTP response to CODE. Internally
ningle's response is an instance of LACK.RESPONSE:RESPONSE. This
function is meant to be used from within handlers. VALUE may be a
number, a keyword or a string representing the HTTP Status Code"
  (let* ((search-by (etypecase value
                      (keyword :key)
                      (number :code)
                      (string :text)))
         (item (get-status-code-or-lose value :by search-by)))
    (setf (response-status ningle:*response*) (getf item :code))))

(defun set-response-body (body)
  "Sets the body for the HTTP response to BODY. Internally ningle's
response is an instance of LACK.RESPONSE:RESPONSE. This function is
meant to be used from within handlers."
  (setf (response-body ningle:*response*) body))

(defun redirect (location &optional (code :moved-permanently))
  "Sets the HTTP status code to CODE and Location header to LOCATION"
  (set-response-header :location location)
  (set-response-status code)
  nil)

(defmacro with-json-response (&body body)
  "A helper macro to be used from within HTTP handlers, which sets the
Content-Type to application/json, and encodes the last evaluated
expression of BODY as a JSON object. On error, it will return an
instance of ERROR-RESPONSE with the message defined by the signalled
condition."
  `(handler-case
       (jonathan:to-json
        (progn
          (set-response-header :content-type "application/json")
          (set-response-status 200)
          ,@body))
     (error (condition)
       (set-response-status 500)
       (let ((error-message (format nil "~A" condition)))
         (jonathan:to-json
          (make-instance 'error-response :message error-message))))))

