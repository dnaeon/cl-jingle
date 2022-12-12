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
(defpackage :jingle.demo.client
  (:use :cl)
  (:import-from :dexador)
  (:import-from :quri)
  (:import-from :jonathan)
  (:export
   :client
   :client-scheme
   :client-port
   :client-hostname
   :client-api-prefix
   :make-client
   :make-api-uri
   :ping
   :get-product-by-id
   :get-products-page
   :delete-product-by-id
   :create-new-product))
(in-package :jingle.demo.client)

(defclass client ()
  ((scheme
    :initarg :scheme
    :initform "http"
    :accessor client-scheme
    :documentation "Scheme to use")
   (port
    :initarg :port
    :initform 5000
    :accessor client-port
    :documentation "Port to connect to")
   (hostname
    :initarg :hostname
    :initform (error "Must specify hostname")
    :accessor client-hostname
    :documentation "Hostname to connect to")
   (api-prefix
    :initarg :api-prefix
    :initform "/api/v1"
    :accessor client-api-prefix
    :documentation "API prefix"))
  (:documentation "Base API client for interfacing with the JINGLE.DEMO API endpoints"))

(defun make-client (&rest rest)
  "Creates a new API client"
  (apply #'make-instance 'client rest))

(defmethod make-api-uri ((client client) path &key query-params)
  "Creates an URI to the given API path"
  (quri:make-uri :scheme (client-scheme client)
                 :port (client-port client)
                 :host (client-hostname client)
                 :path (format nil "~a~a" (client-api-prefix client) path)
                 :query query-params))

(defmethod ping ((client client))
  "Ping the remote API endpoint"
  (let ((uri (make-api-uri client "/ping")))
    (jonathan:parse (dexador:get uri))))

(defmethod get-product-by-id ((client client) id)
  "Fetches a product by id"
  (let* ((path (format nil "/product/~A" id))
         (uri (make-api-uri client path)))
    (jonathan:parse (dexador:get uri))))

(defmethod delete-product-by-id ((client client) id)
  "Deletes a product by id"
  (let* ((path (format nil "/product/~A" id))
         (uri (make-api-uri client path)))
    (jonathan:parse (dexador:delete uri))))

(defmethod get-products-page ((client client) &key (from 0) (to 2))
  "Fetches a page of products"
  (let* ((params `(("from" . ,from) ("to" . ,to)))
         (uri (make-api-uri client "/product" :query-params params)))
    (jonathan:parse (dexador:get uri))))

(defmethod create-new-product ((client client) name)
  "Creates a new product with the given name"
  (let* ((payload (jonathan:to-json (list :|name| name)))
         (uri (make-api-uri client "/product"))
         (headers '(("Accept" . "application/json")
                    ("Content-Type" . "application/json"))))
    (jonathan:parse (dexador:post uri :headers headers :content payload))))
