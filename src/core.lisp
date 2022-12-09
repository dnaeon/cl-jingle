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
   :status-code-number
   :explain-status-code)
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
   :install-middleware
   :clear-middlewares
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
   :redirect
   :redirect-route
   :get-request-header
   :get-response-header
   :request-header-is-set-p
   :response-header-is-set-p
   :get-request-param
   :with-json-response
   :with-request-params

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

(defgeneric install-middleware (app middleware)
  (:documentation "Adds a new middleware to the app"))

(defgeneric clear-middlewares (app)
  (:documentation "Removes all middlewares from the app"))

(defgeneric serve-directory (app path root)
  (:documentation "Serves the files from the given root directory"))

(defgeneric redirect-route (app path location &key code)
  (:documentation "Creates a redirection route for the APP at the given PATH to
LOCATION"))

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

(defmethod install-middleware ((app app) middleware)
  "Appends the given MIDDLEWARE to the APP"
  (with-accessors ((middlewares middlewares)) app
    (setf middlewares (append middlewares (list middleware)))))

(defmethod clear-middlewares ((app app))
  "Removes all installed middlewares from the app"
  (setf (middlewares app) nil))

(defmethod static-path ((app app) path root)
  "Installs the LACK.MIDDLEWARE.STATIC:*LACK-MIDDLEWARE-STATIC*
middleware for serving static files at PATH from the given ROOT"
  (install-middleware app
                  (lambda (app)
                    (funcall
                     lack.middleware.static:*lack-middleware-static* app
                     :path (uiop:native-namestring path) :root root))))

(defmethod serve-directory ((app app) path root)
  "Mounts the LACK.APP.DIRECTORY:LACK-APP-DIRECTORY app at the given
PATH, serving files from ROOT"
  ;; The ROOT *MUST* end with a slash, otherwise the
  ;; LACK-APP-DIRECTORY doesn't work as expected.
  ;; Also, the PATH *MUST NOT* end with a slash, but the client *MUST*
  ;; access it using a slash at the end, otherwise the links to the
  ;; files are broken. Weird, right? Let's try to fix that.
  (let* ((path (if (str:ends-with-p "/" path)
                   (subseq path 0 (1- (length path)))
                   path))
         (separator (uiop:directory-separator-for-host))
         (root-native (uiop:native-namestring root))
         (root (if (not (str:ends-with-p (string separator) root-native))
                   (str:concat root-native (string separator))
                   root))
         (dir-app (make-instance 'lack.app.directory:lack-app-directory :root root)))
    ;; Mount the LACK-APP-DIRECTORY app
    (install-middleware app
                    (lambda (app)
                      (funcall
                       lack.middleware.mount:*lack-middleware-mount* app path dir-app)))))

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
  (setf (response-status ningle:*response*) (status-code-number value)))

(defun set-response-body (body)
  "Sets the body for the HTTP response to BODY. Internally ningle's
response is an instance of LACK.RESPONSE:RESPONSE. This function is
meant to be used from within handlers."
  (setf (response-body ningle:*response*) body))

(defun get-request-header (name)
  "Returns two values, first one is a list representing the values of
the requested header NAME, and the second is T or NIL, depending on
whether the header was set."
  ;; LACK.REQUEST:REQUEST-HEADERS stores the headers in lower-case
  (multiple-value-bind (value exists)
      (gethash (string-downcase name) (request-headers ningle:*request*))
    (values (uiop:split-string value :separator '(#\,)) exists)))

(defun %get-response-header-values (name)
  "Collects all values for the response header with the given NAME"
  (loop :for (header-name header-value) :on (response-headers ningle:*response*) :by #'cddr
        :when (string= name header-name) :collect header-value))

(defun get-response-header (name)
  "Returns two values, first one is a list representing the values of
the requested response header NAME, and the second one is T or NIL,
depending on whether the header was set."
  (let ((header-values (%get-response-header-values name)))
    (values header-values (not (null header-values)))))

(defun request-header-is-set-p (name)
  "Predicate for testing whether a given request header is set"
  (multiple-value-bind (values exists)
      (get-request-header name)
    (declare (ignore values))
    exists))

(defun response-header-is-set-p (name)
  "Predicate for testing whether a given response header is set"
  (multiple-value-bind (values exists)
      (get-response-header name)
    (declare (ignore values))
    exists))

(defun redirect (location &optional (code :moved-permanently))
  "Sets the HTTP status code to CODE and Location header to LOCATION"
  (set-response-header :location location)
  (set-response-status code)
  nil)

(defmethod redirect-route ((app app) path location &key (code :moved-permanently))
  (setf (ningle:route app path :method :get)
        (lambda (params)
          (declare (ignore params))
          ;; RFC 7231 says that we should send a payload with a short
          ;; hypertext note with a hyperlink to the different URI.
          ;; Send the body, only if Content-Type is not set already.
          (unless (response-header-is-set-p :content-type)
            (let ((body (format nil "<h1><a href=\"~A\">~A</a></h1>" location (explain-status-code code))))
              (set-response-header :content-type "text/html; charset=utf-8")
              (set-response-body body)))
          (redirect location code)))
  (setf (ningle:route app path :method :head)
        (lambda (params)
          (declare (ignore params))
          (set-response-header :content-type "text/html; charset=utf-8")
          (redirect location code))))

(defmacro with-json-response (&body body)
  "A helper macro to be used from within HTTP handlers, which sets the
Content-Type to application/json, and encodes the last evaluated
expression of BODY as a JSON object. On error, it will return an
instance of ERROR-RESPONSE with the message defined by the signalled
condition."
  (let ((error-message-sym (gensym)))
    `(handler-case
         (jonathan:to-json
          (progn
            (set-response-header :content-type "application/json")
            (set-response-status :ok)
            ,@body))
       (error (condition)
         (set-response-status :internal-server-error)
         (let ((,error-message-sym (format nil "~A" condition)))
           (jonathan:to-json
            (make-instance 'error-response :message ,error-message-sym)))))))

(defun get-request-param (params name &optional default)
  "Returns the value associated with the NAME param. If NAME param is
 not set at all, returns DEFAULT."
  (let ((value (cdr (assoc name params :test #'equal))))
    (or value default)))

(defmacro with-request-params (param-items params-alist &body body)
  "A helper macro which binds symbols to values from the request
 parameters, and evalutes BODY.

PARAM-ITEMS is a list of (SYMBOL PARAM-NAME DEFAULT-VALUE) lists,
which represent the symbols to be bound while evaluating the BODY.

PARAMS-ALIST is the alist passed to the jingle handler."
  `(let ,(loop :for item :in param-items
               :for sym-var = (first item)
               :for param-name = (second item)
               :for default-val = (third item)
               :collect (list sym-var `(get-request-param ,params-alist ,param-name ,default-val)))
     ,@body))
