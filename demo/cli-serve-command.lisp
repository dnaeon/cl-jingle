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

(defun serve/options ()
  "Returns the options for the `serve' command"
  (list
   (clingon:make-option :string
                        :description "address to bind to"
                        :long-name "address"
                        :initial-value "127.0.0.1"
                        :env-vars '("ADDRESS")
                        :key :serve-cmd/address)
   (clingon:make-option :integer
                        :description "port to listen on"
                        :long-name "port"
                        :initial-value 5000
                        :env-vars '("PORT")
                        :key :serve-cmd/port)
   (clingon:make-option :boolean
                        :description "run in silent mode, if set to true"
                        :long-name "silent"
                        :initial-value :false
                        :env-vars '("SILENT")
                        :key :serve-cmd/silent-mode)))

(defun serve/handler (cmd)
  "The handler for the `serve' command"
  (let* ((address (clingon:getopt cmd :serve-cmd/address))
         (port (clingon:getopt cmd :serve-cmd/port))
         (silent-mode (clingon:getopt cmd :serve-cmd/silent-mode))
         (middlewares (list lack.middleware.accesslog:*lack-middleware-accesslog*))
         (app (jingle:make-app :middlewares middlewares
                               :address address
                               :port port
                               :silent-mode silent-mode
                               :use-thread nil
                               :debug-mode nil)))
    ;; Register the API endpoints for our app and start it up
    ;; SIGINT signals are already handled by CLACK:CLACKUP
    (jingle.demo.api:register-urls app)
    (jingle:start app)))

(defun serve/command ()
  "Returns the `serve' command"
  (clingon:make-command :name "serve"
                        :description "start the HTTP server"
                        :handler #'serve/handler
                        :options (serve/options)))
