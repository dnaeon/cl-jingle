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
(defpackage :jingle.demo.api
  (:use :cl)
  (:import-from :jingle)
  (:import-from :jonathan)
  (:import-from :local-time)
  (:export
   :register-urls))
(in-package :jingle.demo.api)

(define-condition api-error (jingle:base-http-error)
  ()
  (:documentation "Represents a condition which will be signalled on API errors"))

(defmethod jingle:handle-error ((error api-error))
  "Handles the error and sets up the HTTP error response to be sent to
clients"
  (with-accessors ((code jingle:http-error-code)
                   (body jingle:http-error-body)) error
    (jingle:set-response-status code)
    (jingle:set-response-header :content-type "application/json")
    (jonathan:to-json (list :|error| body))))

(defun throw-not-found-error (message)
  "Throws a 404 (Not Found) HTTP response"
  (error 'api-error :code :not-found :body message))

(defun throw-bad-request-error (message)
  "Throws a 400 (Bad Request) HTTP response"
  (error 'api-error :code :bad-request :body message))

(defun get-int-param (params name &optional default)
  "Gets the NAME parameter from PARAMS and parses it as an integer.
In case of invalid input it will signal a 400 (Bad Request) error"
  (let ((raw (jingle:get-request-param params name default)))
    (typecase raw
      (number raw)
      (null (throw-bad-request-error (format nil "missing value for `~A` param" name)))
      (string (let ((parsed (parse-integer raw :junk-allowed t)))
                (unless parsed
                  (throw-bad-request-error (format nil "invalid value for `~A` param" name)))
                parsed))
      (t (throw-bad-request-error (format nil "unsupported value for `~A` param" name))))))

(defun decode-json-or-throw-bad-request (content)
  "Decodes the given CONTENT or signals a Bad Request condition on errors"
  (handler-case (jonathan:parse content)
    (error ()
      (throw-bad-request-error "invalid JSON content"))))

(defparameter *products*
  '((:|id| 1 :|name| "foo")
    (:|id| 2 :|name| "bar")
    (:|id| 3 :|name| "baz")
    (:|id| 4 :|name| "qux"))
  "The `database' used by our API")

(defun find-product-by-id (id)
  "Finds a product by id"
  (find id
        *products*
        :key (lambda (item) (getf item :|id|))
        :test #'=))

(defun take (items from to)
  "A helper function to return the ITEMS between FROM and TO range"
  (let* ((len (length items))
         (to (if (>= to len) len to)))
    (if (>= from len)
        nil
        (subseq items from to))))

(defun get-product-by-id-handler (params)
  "Handles requests for the /api/v1/product/:id endpoint"
  (jingle:with-json-response
    (let* ((id (get-int-param params :id))
           (product (find-product-by-id id)))
      (unless product
        (throw-not-found-error "product not found"))
      product)))

(defun get-products-page-handler (params)
  "Handles requests for the /api/v1/product endpoint"
  (jingle:with-json-response
    (let ((from (get-int-param params "from" 0))
          (to (get-int-param params "to" 2)))
      (when (or (minusp from) (minusp to))
        (throw-bad-request-error "`from` and `to` must be positive"))
      (take
       (setf *products*
             (sort *products* #'< :key (lambda (item) (getf item :|id|))))
       from to))))

(defclass ping-response ()
  ((message
    :initarg :message
    :initform "pong"
    :documentation "Message to send as part of the response")
   (timestamp
    :initarg :timestamp
    :initform (local-time:now)
    :documentation "Timestamp of the message"))
  (:documentation "A response sent as part of a PING request"))

(defmethod jonathan:%to-json ((object ping-response))
  (with-slots (message timestamp) object
    (jonathan:with-object
      (jonathan:write-key-value "message" message)
      (jonathan:write-key-value "timestamp" timestamp))))

(defun ping-handler (params)
  "Handles requests for /api/v1/ping"
  (declare (ignore params))
  (jingle:with-json-response
    (make-instance 'ping-response)))

(defun delete-product-by-id-handler (params)
  "Handles requests for DELETE /api/v1/product/:id endpoint.
Note that this handler is not thread-safe, since that is outside of
the scope for the demo."
  (jingle:with-json-response
    (let* ((id (get-int-param params :id))
           (product (find-product-by-id id)))
      (unless product
        (throw-not-found-error "product not found"))
      (setf *products* (remove id *products* :test #'= :key (lambda (item) (getf item :|id|))))
      product)))

(defun create-product-handler (params)
  "Handles requests for POST /api/v1/product endpoint. Note that this
handler is not thread-safe, since that is outside of the scope for the
demo."
  (declare (ignore params))
  (jingle:with-json-response
    (flet ((product-exists-p (name)
             (find name *products* :test #'string= :key (lambda (item) (getf item :|name|))))
           (get-next-id ()
             (if (null *products*)
                 1
                 (1+ (apply #'max (mapcar (lambda (item) (getf item :|id|)) *products*))))))
      (let* ((content (babel:octets-to-string (jingle:request-content jingle:*request*)))
             (body (decode-json-or-throw-bad-request content))
             (name (getf body :|name|))
             (new-item (list :|id| (get-next-id) :|name| name)))
        (unless name
          (throw-bad-request-error "must provide product name"))
        (when (product-exists-p name)
          (throw-bad-request-error "product already exists"))
        (push new-item *products*)
        new-item))))

(defparameter *urls*
  `((:method :GET    :path "/api/v1/ping"        :handler ,#'ping-handler)
    (:method :GET    :path "/api/v1/product"     :handler ,#'get-products-page-handler)
    (:method :GET    :path "/api/v1/product/:id" :handler ,#'get-product-by-id-handler)
    (:method :POST   :path "/api/v1/product"     :handler ,#'create-product-handler)
    (:method :DELETE :path "/api/v1/product/:id" :handler ,#'delete-product-by-id-handler))
  "The URLs map of our API")

(defun register-urls (app)
  (loop :for item :in *urls*
        :for path = (getf item :path)
        :for handler = (getf item :handler)
        :for method = (getf item :method) :do
          (setf (jingle:route app path :method method) handler)))
