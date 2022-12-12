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

(in-package :jingle.demo.cli)

(defun list/options ()
  "Returns the options for the `list' command"
  (list
   (clingon:make-option :integer
                        :description "fetch products starting from this offset"
                        :long-name "from"
                        :initial-value 0
                        :key :list-cmd/from)
   (clingon:make-option :integer
                        :description "fetch products up to this offset"
                        :long-name "to"
                        :initial-value 2
                        :key :list-cmd/to)))

(defun list/handler (cmd)
  "The handler for the `list' command"
  (let* ((client (make-api-client-from-opts cmd))
         (from (clingon:getopt cmd :list-cmd/from))
         (to (clingon:getopt cmd :list-cmd/to))
         (items (jingle.demo.client:get-products-page client :from from :to to))
         (table (ascii-table:make-table (list "ID" "NAME") :header "PRODUCTS")))
    (dolist (item items)
      (ascii-table:add-row table (list (getf item :|id|) (getf item :|name|))))
    (ascii-table:display table)))

(defun list/command ()
  "Returns the `list' command"
  (clingon:make-command :name "list"
                        :description "list products"
                        :handler #'list/handler
                        :options (list/options)))
