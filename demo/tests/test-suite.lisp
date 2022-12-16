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
(defpackage :jingle.demo.test
  (:use :cl :rove)
  (:import-from :jingle)
  (:import-from :jingle.demo.api)
  (:import-from :lack.middleware.accesslog)
  (:import-from :dexador)
  (:import-from :jonathan))
(in-package :jingle.demo.test)

(defparameter *test-app* nil
  "The test app")

(defparameter *middlewares*
  (list lack.middleware.accesslog:*lack-middleware-accesslog*)
  "The list of middlewares to install on the test app")

(setup
  ;; Sets up a test HTTP server
  (unless *test-app*
    (setf *test-app* (jingle:make-test-app :middlewares *middlewares*))
    (jingle.demo.api:register-urls *test-app*))
  (format t "Starting up test HTTP server ...~%")
  (jingle:start *test-app*)
  ;; Give it some time to start up the server
  (sleep 1))

(teardown
  ;; Shuts down the test HTTP server
  (when *test-app*
    (format t "Shutting down test HTTP server ... ~%")
    (jingle:stop *test-app*)))

(defun get-header (headers name)
  "A helper function to get the value of a given header"
  ;; Headers are in downcase
  (gethash (string-downcase (princ-to-string name)) headers))

(deftest ping-tests
  (testing "GET /api/v1/ping"
    (let ((uri (jingle:url-for *test-app* "ping"))) ;; reverse url
      (multiple-value-bind (body code headers) (dexador:get uri)
        ;; Check HTTP Status Code
        (ok (= code (jingle:status-code-number :ok))
            "Status code is OK")
        ;; Check response body
        (let ((pong (jonathan:parse body)))
          (ok (string= "pong" (getf pong :|message|)) "Pong response message matches")
          (ok (numberp (getf pong :|timestamp|)) "Got valid timestamp in pong response"))
        ;; Check HTTP headers
        (ok (string= (get-header headers :content-type) "application/json")
            "Content-Type matches")))))

(defun get-product-names (items)
  "A helper function which returns the names of fetched products from the
/api/v1/product endpoint"
  (mapcar (lambda (item)
            (getf item :|name|))
          items))

(deftest get-product-pages-tests
  (testing "GET /api/v1/product - first page of products"
    (let* ((from 0)
           (to 2)
           (uri (jingle:url-for *test-app* "get-products-page" :|from| from :|to| to)) ;; reverse url
           (want-product-names '("foo" "bar"))) ;; products with id 1 and 2
      (multiple-value-bind (body code headers) (dexador:get uri)
        ;; Check status code
        (ok (= code (jingle:status-code-number :ok))
            "Status code is OK")
        ;; Check HTTP headers
        (ok (string= (get-header headers :content-type) "application/json")
            "Content-Type matches")
        ;; Check body
        (ok (equal want-product-names (get-product-names (jonathan:parse body)))
            "Fetched products match"))))

  (testing "GET /api/v1/product - second page of products"
    (let* ((from 2)
           (to 4)
           (uri (jingle:url-for *test-app* "get-products-page" :|from| from :|to| to)) ;; reverse url
           (want-product-names '("baz" "qux"))) ;; products with id 3 and 4
      (multiple-value-bind (body code headers) (dexador:get uri)
        ;; Check status code
        (ok (= code (jingle:status-code-number :ok))
            "Status code is OK")
        ;; Check HTTP headers
        (ok (string= (get-header headers :content-type) "application/json")
            "Content-Type matches")
        ;; Check body
        (ok (equal want-product-names (get-product-names (jonathan:parse body)))
            "Fetched products match"))))

  (testing "GET /api/v1/product - third page of products (empty)"
    (let* ((from 100)  ;; we don't have products at this offset
           (to 200)    ;; we don't have products at this offset
           (uri (jingle:url-for *test-app* "get-products-page" :|from| from :|to| to)) ;; reverse url
           (want-product-names nil))
      (multiple-value-bind (body code headers) (dexador:get uri)
        ;; Check status code
        (ok (= code (jingle:status-code-number :ok))
            "Status code is OK")
        ;; Check HTTP headers
        (ok (string= (get-header headers :content-type) "application/json")
            "Content-Type matches")
        ;; Check body
        (ok (equal want-product-names (get-product-names (jonathan:parse body)))
            "Fetched products match"))))

  (testing "GET /api/v1/product - invalid FROM and TO query params"
    (let* ((from "here")
           (to "there")
           (uri (jingle:url-for *test-app* "get-products-page" :|from| from :|to| to))) ;; reverse url
      (handler-case (dexador:get uri)
        (dexador:http-request-failed (e)
          ;; Check status code
          (ok (= (dexador:response-status e) (jingle:status-code-number :bad-request))
              "Status code is Bad Request")
          ;; Check HTTP headers
          (ok (string= (get-header (dexador:response-headers e) :content-type) "application/json")
              "Content-Type matches"))))))

(deftest get-product-by-id-tests
  (testing "GET /api/v1/product/:id - get existing product"
    (let* ((id 1)
           (uri (jingle:url-for *test-app* "get-product-by-id" :id id)) ;; reverse url
           (want-product '(:|id| 1 :|name| "foo"))) ;; products with id 1
      (multiple-value-bind (body code headers) (dexador:get uri)
        ;; Check status code
        (ok (= code (jingle:status-code-number :ok))
            "Status code is OK")
        ;; Check HTTP headers
        (ok (string= (get-header headers :content-type) "application/json")
            "Content-Type matches")
        ;; Check body
        (let ((got-product (jonathan:parse body)))
          (ok (= (getf want-product :|id|) (getf got-product :|id|))
              "Product IDs match")
          (ok (string= (getf want-product :|name|) (getf got-product :|name|))
              "Product names match")))))

  (testing "GET /api/v1/product/:id - get non-existing product"
    (let* ((id 42) ;; non-existing product id
           (uri (jingle:url-for *test-app* "get-product-by-id" :id id))) ;; reverse url
      (handler-case (dexador:get uri)
        (dexador:http-request-failed (e)
          ;; Check status code
          (ok (= (dexador:response-status e) (jingle:status-code-number :not-found))
              "Status code is Not Found")
          ;; Check HTTP headers
          (ok (string= (get-header (dexador:response-headers e) :content-type) "application/json")
              "Content-Type matches")
          ;; Check body
          (let ((api-error (jonathan:parse (dexador:response-body e))))
            (ok (string= (getf api-error :|error|) "product not found")
                "Error message matches"))))))

  (testing "GET /api/v1/product/:id - invalid ID"
    (let* ((id "invalid")
           (uri (jingle:url-for *test-app* "get-product-by-id" :id id))) ;; reverse url
      (handler-case (dexador:get uri)
        (dexador:http-request-failed (e)
          ;; Check status code
          (ok (= (dexador:response-status e) (jingle:status-code-number :bad-request))
              "Status code is Bad Request")
          ;; Check HTTP headers
          (ok (string= (get-header (dexador:response-headers e) :content-type) "application/json")
              "Content-Type matches"))))))

(deftest create-product-by-id-tests
  (testing "POST /api/v1/product/:id"
    (let* ((new-product-name "new-product")
           (payload (jonathan:to-json (list :|name| new-product-name)))
           (headers '(("Accept" . "application/json")
                      ("Content-Type" . "application/json")))
           (uri (jingle:url-for *test-app* "create-product"))) ;; reverse url
      ;; Create the product for the first time
      (multiple-value-bind (body code headers) (dexador:post uri :headers headers :content payload)
        ;; Check status code
        (ok (= code (jingle:status-code-number :ok))
            "Status code is OK")
        ;; Check HTTP headers
        (ok (string= (get-header headers :content-type) "application/json")
            "Content-Type matches")
        ;; Check body
        (let ((product (jonathan:parse body)))
          (ok (string= (getf product :|name|) new-product-name)
              "Product name matches")
          (ok (numberp (getf product :|id|)) "Got valid product ID")))

      ;; Try to create the same product again, which should fail.
      (handler-case (dexador:post uri :headers headers :content payload)
        (dexador:http-request-failed (e)
          ;; Check status code
          (ok (= (dexador:response-status e) (jingle:status-code-number :bad-request))
              "Status code is Bad Request")
          ;; Check HTTP headers
          (ok (string= (get-header (dexador:response-headers e) :content-type) "application/json")
              "Content-Type matches")
          ;; Check body
          (let ((api-error (jonathan:parse (dexador:response-body e))))
            (ok (string= (getf api-error :|error|) "product already exists")
                "Error message matches"))))))

  (testing "POST /api/v1/product/:id - missing NAME in payload"
    (let* ((payload (jonathan:to-json (list :|required-field-is-missing| "new product")))
           (headers '(("Accept" . "application/json")
                      ("Content-Type" . "application/json")))
           (uri (jingle:url-for *test-app* "create-product"))) ;; reverse url
      (handler-case (dexador:post uri :headers headers :content payload)
        (dexador:http-request-failed (e)
          ;; Check status code
          (ok (= (dexador:response-status e) (jingle:status-code-number :bad-request))
              "Status code is Bad Request")
          ;; Check HTTP headers
          (ok (string= (get-header (dexador:response-headers e) :content-type) "application/json")
              "Content-Type matches")
          ;; Check body
          (let ((api-error (jonathan:parse (dexador:response-body e))))
            (ok (string= (getf api-error :|error|) "must provide product name")
                "Error message matches")))))))

(deftest delete-product-by-id-tests
  (testing "DELETE /api/v1/product/:id"
    (let* ((id 1) ;; product with name `foo`
           (uri (jingle:url-for *test-app* "delete-product-by-id" :id id))) ;; reverse url
      ;; Delete the product
      (multiple-value-bind (body code headers) (dexador:delete uri)
        ;; Check status code
        (ok (= code (jingle:status-code-number :ok))
            "Status code is OK")
        ;; Check HTTP headers
        (ok (string= (get-header headers :content-type) "application/json")
            "Content-Type matches")
        ;; Check body
        (let ((product (jonathan:parse body)))
          (ok (string= (getf product :|name|) "foo")
              "Product name matches")
          (ok (= (getf product :|id|) 1) "Product IDs match")))

      ;; Try to delete it again, which should fail.
      (handler-case (dexador:delete uri)
        (dexador:http-request-failed (e)
          ;; Check status code
          (ok (= (dexador:response-status e) (jingle:status-code-number :not-found))
              "Status code is Not Found")
          ;; Check HTTP headers
          (ok (string= (get-header (dexador:response-headers e) :content-type) "application/json")
              "Content-Type matches")
          ;; Check body
          (let ((api-error (jonathan:parse (dexador:response-body e))))
            (ok (string= (getf api-error :|error|) "product not found")
                "Error message matches"))))))

  (testing "DELETE /api/v1/product/:id - with invalid ID value"
    (let* ((id "this-is-a-bad-id")
           (uri (jingle:url-for *test-app* "delete-product-by-id" :id id))) ;; reverse url
      (handler-case (dexador:delete uri)
        (dexador:http-request-failed (e)
          ;; Check status code
          (ok (= (dexador:response-status e) (jingle:status-code-number :bad-request))
              "Status code is Bad Request")
          ;; Check HTTP headers
          (ok (string= (get-header (dexador:response-headers e) :content-type) "application/json")
              "Content-Type matches"))))))
