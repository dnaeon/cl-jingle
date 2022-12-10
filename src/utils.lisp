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
(defpackage :jingle.utils
  (:use :cl)
  (:import-from
   :jingle.codes
   :status-code-number)
  (:import-from :ningle)
  (:import-from :lack.request)
  (:import-from :lack.response)
  (:import-from :local-time)
  (:import-from :jonathan)
  (:export
   :set-response-header
   :set-response-status
   :set-response-body
   :get-request-header
   :get-response-header
   :request-header-is-set-p
   :response-header-is-set-p
   :get-request-param
   :redirect
   :with-json-response
   :with-html-response
   :with-request-params))
(in-package :jingle.utils)

(defmethod jonathan:%to-json ((timestamp local-time:timestamp))
  (jonathan:%to-json (local-time:timestamp-to-unix timestamp)))

(defun set-response-header (name value)
  "Sets the HTTP header with the given NAME to VALUE to be sent to the
client as part of the HTTP response. Internally ningle's response is
an instance of LACK.RESPONSE:RESPONSE. This function is meant to be
used from within handlers."
  (setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*)
                (list name value))))

(defun set-response-status (value)
  "Sets the status code for the HTTP response to CODE. Internally
ningle's response is an instance of LACK.RESPONSE:RESPONSE. This
function is meant to be used from within handlers. VALUE may be a
number, a keyword or a string representing the HTTP Status Code"
  (setf (lack.response:response-status ningle:*response*) (status-code-number value)))

(defun set-response-body (body)
  "Sets the body for the HTTP response to BODY. Internally ningle's
response is an instance of LACK.RESPONSE:RESPONSE. This function is
meant to be used from within handlers."
  (setf (lack.response:response-body ningle:*response*) body))

(defun get-request-header (name)
  "Returns two values, first one is a list representing the values of
the requested header NAME, and the second is T or NIL, depending on
whether the header was set."
  ;; LACK.REQUEST:REQUEST-HEADERS stores the headers in lower-case
  (multiple-value-bind (value exists)
      (gethash (string-downcase name) (lack.request:request-headers ningle:*request*))
    (values (uiop:split-string value :separator '(#\,)) exists)))

(defun %get-response-header-values (name)
  "Collects all values for the response header with the given NAME"
  (loop :for (header-name header-value) :on (lack.response:response-headers ningle:*response*) :by #'cddr
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

(defun get-request-param (params name &optional default)
  "Returns the value associated with the NAME param. If NAME param is
 not set, returns DEFAULT."
  (let ((value (cdr (assoc name params :test #'equal))))
    (or value default)))

(defun redirect (location &optional (code :moved-permanently))
  "Sets the HTTP status code to CODE and Location header to LOCATION"
  (set-response-header :location location)
  (set-response-status code)
  nil)

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

(defmacro with-html-response (&body body)
  "A helper macro to be used from within HTTP handlers, which sets the
Content-Type to text/html. On error, it will return a response with
status code 500 (Internal Server Error) and a short HTML body
describing the condition, which was signalled."
  (let ((error-message-sym (gensym)))
    `(handler-case
         (progn
           (set-response-header :content-type "text/html; charset=utf-8")
           (set-response-status :ok)
           ,@body)
       (error (condition)
         (set-response-status :internal-server-error)
         (let ((,error-message-sym (format nil "~A" condition)))
           (format nil "
<html>
  <head>
    <title>Internal Server Error</title>
  </head>
  <body>
    <h1>Internal Server Error</h1>
    <h2>~A</h2>
  </body>
</html>" ,error-message-sym))))))

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
