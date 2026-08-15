;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq inhibit-startup-screen t)
(column-number-mode 1)

;; does not revert scroll direction :(
(setq mouse-wheel-up-event 'wheel-down
      mouse-wheel-down-event 'wheel-up)
;; Enable horizontal scrolling via trackpad or tilted mouse wheel
(setq mouse-wheel-tilt-scroll t)
;; Flip the direction to make it feel natural
(setq mouse-wheel-flip-direction t)


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)
(package-initialize)

(use-package osx-clipboard
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (osx-clipboard-mode +1))

(save-place-mode 1)
(setq desktop-save t)
(desktop-save-mode 1)

(use-package ri
  :load-path "~/personal_projects/emacs/ri-mode"
  :config
  (ri-enable))

;; Needed when loading ri locally
(use-package kkp
  :ensure t
  :hook (tty-setup . global-kkp-mode)) 

;;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'rf-tree-sitter)
(require 'rf-spell)
;(require 'rf-require)
;(require 'rf-init)
;(require 'rf-maximized)
;(require 'rf-custom)
;(require 'rf-current-time)
;(require 'rf-dashboard)
;(require 'rf-zig)
;(require 'rf-tsi)
;(require 'rf-prettier)
;(require 'rf-multiple-cursors)
;(require 'rf-mwim)
;(require 'rf-dired)
;(require 'rf-compile)
;(require 'rf-expand-region)
;(require 'rf-which-func)
;(require 'rf-ibuffer)
;(require 'rf-json)
;(require 'rf-smartparens)
;(require 'rf-misc)
;(require 'rf-help)
;(require 'rf-evil)
;(require 'rf-meow)
;(require 'rf-font)
