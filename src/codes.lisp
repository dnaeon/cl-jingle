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
(defpackage :jingle.codes
  (:use :cl)
  (:export
   :*status-codes*
   :lookup-status-code
   :lookup-status-code-or-lose
   :explain-status-code))
(in-package :jingle.codes)

(defparameter *status-codes*
  ;; Informational
  '((:code 100 :key :continue :text "Continue")  ;; RFC9110, Section 15.2.1
    (:code 101 :key :switching-protocols :text "Switching Protocols")  ;; RFC9110, Section 15.2.2
    (:code 102 :key :processing :text "Processing")  ;; RFC2518
    (:code 103 :key :early-hints :text "Early Hints")  ;; RFC8297

    ;; Success
    (:code 200 :key :ok :text "OK")  ;; RFC9110, Section 15.3.1
    (:code 201 :key :created :text "Created")  ;; RFC9110, Section 15.3.2
    (:code 202 :key :accepted :text "Accepted")  ;; RFC9110, Section 15.3.3
    (:code 203 :key :non-authoritative-information :text "Non-Authoritative Information")  ;; RFC9110, Section 15.3.4
    (:code 204 :key :no-content :text "No Content")  ;; RFC9110, Section 15.3.5
    (:code 205 :key :reset-content :text "Reset Content")  ;; RFC9110, Section 15.3.6
    (:code 206 :key :partial-content :text "Partial Content")  ;; RFC9110, Section 15.3.7
    (:code 207 :key :multi-status :text "Multi-Status")  ;; RFC4918
    (:code 208 :key :already-reported :text "Already Reported")  ;; RFC5842
    (:code 226 :key :im-used :text "IM Used")  ;; RFC3229

    ;; Redirection
    (:code 300 :key :multiple-choices :text "Multiple Choices")  ;; RFC9110, Section 15.4.1
    (:code 301 :key :moved-permanently :text "Moved Permanently")  ;; RFC9110, Section 15.4.2
    (:code 302 :key :found :text "Found")  ;; RFC9110, Section 15.4.3
    (:code 303 :key :see-other :text "See Other")  ;; RFC9110, Section 15.4.4
    (:code 304 :key :not-modified :text "Not Modified")  ;; RFC9110, Section 15.4.5
    (:code 305 :key :use-proxy :text "Use Proxy")  ;; RFC9110, Section 15.4.6
    (:code 307 :key :temporary-redirect :text "Temporary Redirect")  ;; RFC9110, Section 15.4.8
    (:code 308 :key :permanent-redirect :text "Permanent Redirect")  ;; RFC9110, Section 15.4.9

    ;; Client Error
    (:code 400 :key :bad-request :text "Bad Request")  ;; RFC9110, Section 15.5.1
    (:code 401 :key :unauthorized :text "Unauthorized")  ;; RFC9110, Section 15.5.2
    (:code 402 :key :payment-required :text "Payment Required")  ;; RFC9110, Section 15.5.3
    (:code 403 :key :forbidden :text "Forbidden")  ;; RFC9110, Section 15.5.4
    (:code 404 :key :not-found :text "Not Found")  ;; RFC9110, Section 15.5.5
    (:code 405 :key :method-not-allowed :text "Method Not Allowed")  ;; RFC9110, Section 15.5.6
    (:code 406 :key :not-acceptable :text "Not Acceptable")  ;; RFC9110, Section 15.5.7
    (:code 407 :key :proxy-authentication-required :text "Proxy Authentication Required")  ;; RFC9110, Section 15.5.8
    (:code 408 :key :request-timeout :text "Request Timeout")  ;; RFC9110, Section 15.5.9
    (:code 409 :key :conflict :text "Conflict")  ;; RFC9110, Section 15.5.10
    (:code 410 :key :gone :text "Gone")  ;; RFC9110, Section 15.5.11
    (:code 411 :key :length-required :text "Length Required")  ;; RFC9110, Section 15.5.12
    (:code 412 :key :precondition-failed :text "Precondition Failed")  ;; RFC9110, Section 15.5.13
    (:code 413 :key :content-too-large :text "Content Too Large")  ;; RFC9110, Section 15.5.14
    (:code 414 :key :uri-too-long :text "URI Too Long")  ;; RFC9110, Section 15.5.15
    (:code 415 :key :unsupported-media-type :text "Unsupported Media Type")  ;; RFC9110, Section 15.5.16
    (:code 416 :key :range-not-satisfiable :text "Range Not Satisfiable")  ;; RFC9110, Section 15.5.17
    (:code 417 :key :expectation-failed :text "Expectation Failed")  ;; RFC9110, Section 15.5.18
    (:code 418 :key :teapot :text "I'm a teapot")  ;; RFC9110, Section 15.5.19 (Unused)
    (:code 421 :key :misdirected-request :text "Misdirected Request")  ;; RFC9110, Section 15.5.20
    (:code 422 :key :unprocessable-content :text "Unprocessable Content")  ;; RFC9110, Section 15.5.21
    (:code 423 :key :locked :text "Locked")  ;; RFC4918
    (:code 424 :key :failed-dependency :text "Failed Dependency")  ;; RFC4918
    (:code 425 :key :too-early :text "Too Early")  ;; RFC8470
    (:code 426 :key :upgrade-required :text "Upgrade Required")  ;; RFC9110, Section 15.5.22
    (:code 428 :key :precondition-required :text "Precondition Required")  ;; RFC6585
    (:code 429 :key :too-many-requests :text "Too Many Requests")  ;; RFC6585
    (:code 431 :key :request-header-fields-too-large :text "Request Header Fields Too Large")  ;; RFC6585
    (:code 451 :key :unavailable-for-legal-reasons :text "Unavailable For Legal Reasons")  ;; RFC7725

    ;; Server Error
    (:code 500 :key :internal-server-error :text "Internal Server Error")  ;; RFC9110, Section 15.6.1
    (:code 501 :key :not-implemented :text "Not Implemented")  ;; RFC9110, Section 15.6.2
    (:code 502 :key :bad-gateway :text "Bad Gateway")  ;; RFC9110, Section 15.6.3
    (:code 503 :key :service-unavailable :text "Service Unavailable")  ;; RFC9110, Section 15.6.4
    (:code 504 :key :gateway-timeout :text "Gateway Timeout")  ;; RFC9110, Section 15.6.5
    (:code 505 :key :http-version-not-supported :text "HTTP Version Not Supported")  ;; RFC9110, Section 15.6.6
    (:code 506 :key :variant-also-negotiates :text "Variant Also Negotiates")  ;; RFC2295
    (:code 507 :key :insufficient-storage :text "Insufficient Storage")  ;; RFC4918
    (:code 508 :key :loop-detected :text "Loop Detected")  ;; RFC5842
    (:code 510 :key :not-extended :text "Not Extended (OBSOLETED)")  ;; RFC2774 ((OBSOLETED)
    (:code 511 :key :network-authentication-required :text "Network Authentication Required"))  ;; RFC6585
  "HTTP Status Code Registry from IANA")

(defun lookup-status-code (value &key (by :code))
  "Returns the HTTP Status Code associated with VALUE by looking up the registry"
  (find value *status-codes* :test #'equal :key (lambda (item) (getf item by))))

(defun lookup-status-code-or-lose (value &key (by :code))
  (let ((item (lookup-status-code value :by by)))
    (unless item
      (error "Unknown HTTP Status Code ~A" value))
    item))
