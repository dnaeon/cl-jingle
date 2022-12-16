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
(defpackage :jingle
  (:use :cl)
  (:import-from :jingle.core)
  (:import-from :ningle)
  (:import-from :find-port))
(in-package :jingle)

(cl-reexport:reexport-from :jingle.codes)
(cl-reexport:reexport-from :jingle.utils)
(cl-reexport:reexport-from :jingle.core)

;; Re-exports from NINGLE
(cl-reexport:reexport-from :ningle :exclude '(:app :<app>))

;; Re-export from LACK.REQUEST
(cl-reexport:reexport-from
 :lack.request :include '(:request-env
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
                          :request-accepts-p))

;; Re-export from LACK.RESPONSE
(cl-reexport:reexport-from
 :lack.response :include '(:response-status
                           :response-headers
                           :response-body
                           :response-set-cookies))

;; Re-exports from FIND-PORT
(cl-reexport:reexport-from :find-port)
