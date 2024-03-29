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
(rf/require 'quelpa)

(provide 'rf-require)