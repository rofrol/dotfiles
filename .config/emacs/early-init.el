;;; early-init.el -*- lexical-binding: t; -*-

;; Log startup stats
;; https://andrewjamesjohnson.com/modernizing-my-emacs-configuration/
;; https://github.com/ajsquared/home-osx/blob/4d03ecfda464ea0dffc2604c894bcfec6b065e32/.emacs.d/early-init.el#L5
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; below slashes startup time from 0.20 to 0.14 sec, so 0.06/0.2 = 30% quicker :D

;; https://raw.githubusercontent.com/doomemacs/doomemacs/master/early-init.el

;; Garbage collection is a big contributor to startup times. This fends it off,
;; then is reset later by enabling `gcmh-mode'. Not resetting it will cause
;; stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Prioritize old byte-compiled source files over newer sources. It saves us a
;; little IO time to skip all the mtime checks on each lookup.
(setq load-prefer-newer nil)

(unless (or (daemonp)
            init-file-debug)
  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay))))

(when (eq system-type 'darwin)
  ;; needs to be in early-init.el when early-init.el exists?
  ;; also does not work when inside (when window-system
  (add-to-list 'default-frame-alist '(undecorated . t))
)