;;; -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (when window-system
    ;; does not work when in early-init.el
    (set-frame-parameter nil 'fullscreen 'maximized)
  )
)

; noticable unmaximized frame, then maximized
; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up/5909#5909
;(add-hook 'window-setup-hook 'toggle-frame-maximized t)

; noticable unmaximized frame, then maximized
; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up/3017#3017
;(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defun rf/toggle-between-fullscreen-and-maximized ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
    (if (eq 'fullboth (frame-parameter (selected-frame) 'fullscreen)) 'maximized 'fullboth)))
(global-set-key (kbd "<f7>") 'rf/toggle-between-fullscreen-and-maximized)

; Make new frame, not initial, also maximized
; https://github.com/creack/dotfiles/blob/fda67ff6b7a2bc1df157747e90a376c08c42f9ff/.emacs.files/graphic.el#L9
;(add-hook 'after-make-frame-functions
;    (lambda (frame)
;      (select-frame frame)
;      (set-frame-parameter nil 'fullscreen 'maximized)))
; shorter
; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up/3008#3008
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'rf-maximized)
