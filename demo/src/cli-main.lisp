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

(defun print-doc/command ()
  "Returns a command which will print the app's documentation"
  (clingon:make-command
   :name "print-doc"
   :description "print the documentation in Markdown format"
   :usage ""
   :handler (lambda (cmd)
              ;; Print the documentation starting from the parent
              ;; command, so we can traverse all sub-commands in the
              ;; tree.
              (clingon:print-documentation :markdown (clingon:command-parent cmd) t))))

(defun zsh-completions/command ()
  "Returns a command for generating Zsh completion script"
  (clingon:make-command
   :name "zsh-completions"
   :description "generate the Zsh completion script"
   :usage ""
   :handler (lambda (cmd)
              ;; Use the parent command when generating the completions,
              ;; so that we can traverse all sub-commands in the tree.
              (let ((parent (clingon:command-parent cmd)))
                (clingon:print-documentation :zsh-completions parent t)))))

(defun top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (serve/command)
   (print-doc/command)
   (zsh-completions/command)))

(defun top-level/handler (cmd)
  "The handler for the top-level command"
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  "Returns the top-level command"
  (clingon:make-command :name "jingle-demo"
                        :version "0.1.0"
                        :description "The jingle demo app"
                        :authors '("Marin Atanasov Nikolov <dnaeon@gmail.com>")
                        :license "BSD 2-Clause"
                        :handler #'top-level/handler
                        :sub-commands (top-level/sub-commands)))

(defun main ()
  "The main entrypoint of our demo app"
  (let ((app (top-level/command)))
    (clingon:run app)))
