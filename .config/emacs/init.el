;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'rf-require)
(require 'rf-init)
(require 'rf-maximized)
(require 'rf-custom)
(require 'rf-current-time)
(require 'rf-dashboard)
(require 'rf-misc)

(require 'rf-tree-sitter)
;; (require 'rf-tsi)
(require 'rf-prettier)
(require 'rf-multiple-cursors)

(require 'rf-mwim)
(require 'rf-dired)
(require 'rf-zig)

(require 'rf-compile)
(require 'rf-expand-region)
(require 'rf-which-func)
(require 'rf-ibuffer)
(require 'rf-json)
(require 'rf-smartparens)
