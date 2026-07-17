#!/usr/bin/env guile
!#

;; -*- mode: scheme; -*-

;;; guix-style.scm - Formatter hook for Guix Scheme files.
;;; Usage: cat file.scm | guix-style.scm <host> <port>

(use-modules (ice-9 rdelim)
             (ice-9 textual-ports))

(let* ((args (command-line))
       (host (cadr args))
       (port (caddr args))
       (tmp-local (tmpnam))
       (remote-path (string-append "/tmp/guix-style-" (number->string (getpid)) ".scm")))

  ;; 1. Read stdin (buffer content from editor) to local temp file
  (call-with-output-file tmp-local
    (lambda (out-port)
      (display (get-string-all (current-input-port)) out-port)))

  ;; 2. Transfer file, format on remote, and fetch back
  (system* "scp" "-q" "-P" port tmp-local (string-append host ":" remote-path))
  (system* "ssh" "-p" port host (string-append "guix style --whole-file " remote-path))
  (system* "scp" "-q" "-P" port (string-append host ":" remote-path) tmp-local)

  ;; 3. Output formatted result to stdout
  (display (call-with-input-file tmp-local get-string-all))

  ;; 4. Cleanup
  (delete-file tmp-local)
  (system* "ssh" "-p" port host (string-append "rm -f " remote-path)))
