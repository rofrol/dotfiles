;; https://github.com/rexim/dotfiles/blob/a529f79ffe3bac19fe1ce842c3296ad792757df7/.emacs.rc/rc.el#L1

;; without below error: Symbol's value as variable is void: "package-archives"
;; https://www.reddit.com/r/emacs/comments/e990jg/comment/fah8qer/
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(provide 'rf-require)