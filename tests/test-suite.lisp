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
(defpackage :jingle.test
  (:use :cl :rove)
  (:import-from :jingle))
(in-package :jingle.test)

(deftest http-status-codes
  (testing "lookup-status-code :by :code"
    (let ((code-ok (jingle:lookup-status-code 200 :by :code))
          (code-br (jingle:lookup-status-code 400 :by :code))
          (code-ise (jingle:lookup-status-code 500 :by :code)))
      (ok (equal code-ok '(:code 200 :key :ok :text "OK"))
          "Lookup HTTP Status Code 200")
      (ok (equal code-br '(:code 400 :key :bad-request :text "Bad Request"))
          "Lookup HTTP Status Code 400")
      (ok (equal code-ise '(:code 500 :key :internal-server-error :text "Internal Server Error"))
          "Lookup HTTP Status Code 500")))

  (testing "lookup-status-code :by :key"
    (let ((code-ok (jingle:lookup-status-code :ok :by :key))
          (code-br (jingle:lookup-status-code :bad-request :by :key))
          (code-ise (jingle:lookup-status-code :internal-server-error :by :key)))
      (ok (equal code-ok '(:code 200 :key :ok :text "OK"))
          "Lookup HTTP Status Code 200")
      (ok (equal code-br '(:code 400 :key :bad-request :text "Bad Request"))
          "Lookup HTTP Status Code 400")
      (ok (equal code-ise '(:code 500 :key :internal-server-error :text "Internal Server Error"))
          "Lookup HTTP Status Code 500")))

  (testing "lookup-status-code :by :text"
    (let ((code-ok (jingle:lookup-status-code "OK" :by :text))
          (code-br (jingle:lookup-status-code "Bad Request" :by :text))
          (code-ise (jingle:lookup-status-code "Internal Server Error" :by :text)))
      (ok (equal code-ok '(:code 200 :key :ok :text "OK"))
          "Lookup HTTP Status Code 200")
      (ok (equal code-br '(:code 400 :key :bad-request :text "Bad Request"))
          "Lookup HTTP Status Code 400")
      (ok (equal code-ise '(:code 500 :key :internal-server-error :text "Internal Server Error"))
          "Lookup HTTP Status Code 500")))

  (testing "status-code-number"
    (ok (= 200 (jingle:status-code-number :ok)) "Status code number for :ok")
    (ok (= 202 (jingle:status-code-number :accepted)) "Status code number for :accepted")
    (ok (= 500 (jingle:status-code-number 500)) "Status code number for 500")
    (ok (signals (jingle:status-code-number :unknown-status-code)) "Signals on :unknown-status-code"))

  (testing "explain-status-code"
    (ok (string= "OK" (jingle:explain-status-code :ok)) "Status code text for :ok")
    (ok (string= "Bad Request" (jingle:explain-status-code :bad-request)) "Status code text for :bad-request")
    (ok (string= "Internal Server Error" (jingle:explain-status-code 500)) "Status code text for 500")
    (ok (signals (jingle:explain-status-code :unknown-status-code)) "signals on :unknown-status-code"))

  (testing "informational-code-p"
    (ok (jingle:informational-code-p :continue) "Code :continue is informational")
    (ok (jingle:informational-code-p :early-hints) "Code :early-hints is informational")
    (ng (jingle:informational-code-p :ok) "Code :ok is not informational"))

  (testing "success-code-p"
    (ok (jingle:success-code-p :ok) "Code :ok is success")
    (ok (jingle:success-code-p :accepted) "Code :accepted is success")
    (ng (jingle:success-code-p :bad-request) "Code :bad-request is not success"))

  (testing "redirection-code-p"
    (ok (jingle:redirection-code-p :moved-permanently) "Code :moved-permanently is redirection")
    (ok (jingle:redirection-code-p 302) "Code 302 is redirection")
    (ng (jingle:redirection-code-p 400) "Code 400 is not redirection"))

  (testing "client-error-code-p"
    (ok (jingle:client-error-code-p 400) "Code 400 is client-error-code")
    (ok (jingle:client-error-code-p :unauthorized) "Code :unauthorized is client-error-code")
    (ng (jingle:client-error-code-p 500) "Code 500 is not client-error-code"))

  (testing "server-error-code-p"
    (ok (jingle:server-error-code-p :bad-gateway) "Code :bad-gateway is server-error-code")
    (ok (jingle:server-error-code-p 500) "Code 500 is server-error-code")
    (ng (jingle:server-error-code-p :ok) "Code :ok is not server-error-code"))

  (testing "status-code-kind"
    (ok (equal :informational (jingle:status-code-kind :continue)) "Code :continue is informational")
    (ok (equal :success (jingle:status-code-kind :ok)) "Code :ok is success")
    (ok (equal :redirection (jingle:status-code-kind 301)) "Code 301 is redirection")
    (ok (equal :client-error (jingle:status-code-kind :bad-request)) "Code :bad-request is client-error")
    (ok (equal :server-error (jingle:status-code-kind 500)) "Code 500 is server-error")))

