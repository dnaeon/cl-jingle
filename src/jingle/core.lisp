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
(defpackage :cl-playground.jingle.core
  (:use :cl)
  (:nicknames :playground.jingle.core)
  (:import-from :ningle)
  (:import-from :lack)
  (:import-from :clack)
  (:import-from :local-time)
  (:import-from :jonathan)
  (:export
   :*env*
   :start
   :stop
   :error-response
   :error-response-message
   :app
   :app-server
   :app-middlewares
   :make-app
   :set-response-header
   :set-response-status
   :with-json-response))
(in-package :cl-playground.jingle.core)

(defgeneric start (app)
  (:documentation "Starts the jingle application and serves requests"))

(defgeneric stop (app)
  (:documentation "Stops the jingle application and the underlying HTTP server"))

(defparameter *env* nil
  "*ENV* will be dynamically bound to the Lack environment. It can be
used to query the environment from within the HTTP handlers.")

(defmethod jonathan:%to-json ((timestamp local-time:timestamp))
  (jonathan:%to-json (local-time:timestamp-to-unix timestamp)))
