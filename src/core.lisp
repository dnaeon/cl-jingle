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
  (:import-from
   :jingle.utils
   :set-response-body
   :set-response-header
   :set-response-status
   :response-header-is-set-p
   :redirect)
  (:import-from :lack)
  (:import-from :lack.component)
  (:import-from :lack.middleware.static)
  (:import-from :lack.middleware.mount)
  (:import-from :lack.app.directory)
  (:import-from :ningle)
  (:import-from :clack)
  (:export
   ;; Special vars
   :*env*
   ;; Generics
   :start
   :stop
   :configure
   :static-path
   :install-middleware
   :clear-middlewares
   :serve-directory
   :redirect-route
   :handle-error
   :find-route
   :url-for
   ;; Conditions
   :base-http-error
   :http-error-code
   :http-error-body
   ;; App class and accessors
   :app
   :make-app
   :http-server
   :http-server-kind
   :middlewares
   :address
   :port
   :debug-mode
   :silent-mode
   :use-thread))
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

(defgeneric handle-error (condition)
  (:documentation "Handles the CONDITION by setting up appropriate HTTP response to send
to the client"))

(defgeneric find-route (app name)
  (:documentation "Finds and returns the route with the given name"))

(defgeneric url-for (app name &rest params)
  (:documentation "Returns the URL path for the given route NAME with all PARAMS applied
to it"))

(define-condition base-http-error (simple-error)
  ((code
    :initarg :code
    :initform (error "Must specify the HTTP Status Code")
    :accessor http-error-code
    :documentation "The HTTP Status Code")
   (body
    :initarg :body
    :initform nil
    :accessor http-error-body
    :documentation "The body to send as part of the HTTP response"))
  (:documentation "Base condition class for HTTP errors")
  (:report (lambda (condition stream)
             (with-slots (code) condition
               (let* ((code (status-code-number code))
                      (text (or (explain-status-code code) "Unknown")))
               (format stream "~A (~A)" code text))))))

(defmethod handle-error ((error base-http-error))
  (with-slots (code body) error
    (set-response-header :content-type "text/plain")
    (set-response-status code)
    (set-response-body body)))

(defparameter *env* nil
  "*ENV* will be dynamically bound to the Lack environment. It can be
used to query the environment from within the HTTP handlers.")

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
   (use-thread
    :initarg :use-thread
    :initform t
    :accessor use-thread
    :documentation "Start server in a separate thread, if set to T"))
  (:documentation "The jingle app"))

(defun make-app (&key (middlewares nil)
                   (server-kind :hunchentoot)
                   (address "127.0.0.1")
                   (port 5000)
                   (debug-mode t)
                   (silent-mode nil)
                   (use-thread t))
  "Creates a new jingle app"
  (make-instance 'app
                 :middlewares middlewares
                 :http-server-kind server-kind
                 :address address
                 :port port
                 :debug-mode debug-mode
                 :silent-mode silent-mode
                 :use-thread use-thread))

(defmethod lack.component:call ((app app) env)
  "Dynamically binds *ENV* to the Lack environment before it hits the
HTTP handlers.  This way the HTTP handlers can interact with the
surrounding environment exposed by Lack by using JINGLE.CORE:*ENV*.

If a condition is signalled and the condition is a sub-class of
JINGLE:BASE-HTTP-ERROR, then invoke JINGLE:HANDLE-ERROR for setting up
an appropriate HTTP response for the client."
  (let ((*env* env))
    (handler-case (call-next-method)
      (base-http-error (condition)
        (handle-error condition)))))

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
                   (use-thread use-thread)) app
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
                           :use-thread use-thread)))))

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
