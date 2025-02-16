;;; -*- lexical-binding: t; -*-

;; https://github.com/rexim/dotfiles/blob/a529f79ffe3bac19fe1ce842c3296ad792757df7/.emacs.rf/rc.el#L1

;; without below error: Symbol's value as variable is void: "package-archives"
;; https://www.reddit.com/r/emacs/comments/e990jg/comment/fah8qer/
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar rf/package-contents-refreshed nil)

(defun rf/package-refresh-contents-once ()
  (when (not rf/package-contents-refreshed)
    (setq rf/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rf/require-one-package (package)
  (when (not (package-installed-p package))
    (rf/package-refresh-contents-once)
    (package-install package)))

(defun rf/require (&rest packages)
  (dolist (package packages)
    (rf/require-one-package package)))

;; put packages configuration ~/.config/emacs/etc.
(use-package no-littering
  :ensure t
  :demand t)

;; Bootstrap `quelpa'.
;; https://github.com/condy0919/.emacs.d/blob/ba3c48f6064e4756dec32c0976eab00e81facb80/init.el#L46
;; https://github.com/forrestchang/doom-emacs/blob/66ddbf981e56c4503a31e463b93f09b1b91c3be9/core/core-packages.el#L74
;; :comands
;;   It does not load quelpa at startup.
;;   It registers the command quelpa for autoloading.
;;   The package will only be loaded when quelpa is actually invoked.
;; https://github.com/quelpa/quelpa
;; disable auto-upgrade for startup performance
;; call y/upgrade-quelpa manually if necessary
;; :pin
;; - It ensures that quelpa is sourced from the specified repository (melpa in this case), even if the package is available from multiple sources (e.g., GNU ELPA, MELPA, MELPA Stable, or a custom repository).
;; - This is useful if you want to override the default repository priority or prevent installation from an unwanted source.
;; :ensure
;;   Ensures that the package is installed automatically if it is not already installed.
;; :demand
;;   Purpose: Forces the package to be loaded immediately when Emacs starts, instead of deferring it until it is actually needed.
(use-package quelpa
  :ensure t
  :pin melpa
  :commands quelpa)
  ;:custom
  ;;; Don't track MELPA, we'll use package.el for that
  ;(quelpa-git-clone-depth 1)
  ;(quelpa-self-upgrade-p nil)
  ;(quelpa-update-melpa-p nil)
  ;(quelpa-checkout-melpa-p nil)
  ;(quelpa-melpa-recipe-stores nil))

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; https://emacs.stackexchange.com/questions/62036/installing-quelpa-use-package-from-use-package/79563#79563
(setq use-package-always-ensure t)

(require 'use-package-ensure)

;(setq use-package-ensure-function 'quelpa)
(setq use-package-always-ensure t)

(use-package quelpa-use-package
  :demand
  :config
  (quelpa-use-package-activate-advice))

(provide 'rf-require)
