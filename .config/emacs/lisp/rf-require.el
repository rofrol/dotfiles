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

;; install from local or github etc.

;; https://emacs.stackexchange.com/questions/62036/installing-quelpa-use-package-from-use-package/79563#79563
(setq use-package-always-ensure t)

(require 'use-package-ensure)

(use-package quelpa
  :ensure t)

;(setq use-package-ensure-function 'quelpa)
(setq use-package-always-ensure t)

(use-package quelpa-use-package
  :demand
  :config
  (quelpa-use-package-activate-advice))

(provide 'rf-require)
