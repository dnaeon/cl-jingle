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

(defpackage :jingle-system
  (:use :cl :asdf))
(in-package :jingle-system)

(defsystem "jingle"
  :name "jingle"
  :long-name "jingle"
  :description "jingle -- ningle with bells and whistles"
  :version "0.2.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.org"))
  :homepage "https://github.com/dnaeon/cl-jingle"
  :bug-tracker "https://github.com/dnaeon/cl-jingle"
  :source-control "https://github.com/dnaeon/cl-jingle"
  :depends-on (:str
               :quri
               :myway
               :lack
               :lack-middleware-static
               :lack-middleware-mount
               :lack-app-directory
               :clack
               :ningle
               :cl-reexport
               :local-time
               :jonathan
               :find-port)
  :components ((:module "jingle"
                :pathname #P"src/"
                :components ((:file "codes")
                             (:file "utils")
                             (:file "core" :depends-on ("codes" "utils"))
                             (:file "package" :depends-on ("codes" "utils" "core"))))))
