;;; -*- lexical-binding: t; -*-

;(set-face-attribute 'mode-line nil :height 120)  ;; Adjust the height percentage
;(set-face-attribute 'mode-line-inactive nil :height 120)  ;; For inactive windows
;(set-face-attribute 'minibuffer-prompt nil :height 120)  ;; Adjust percentage as needed
;(set-frame-font "Miosevka Nerd Font 16")  ; Replace "Monaco" with your preferred font and "12" with your desired size
;(set-face-attribute 'mode-line nil :font "Miosevka Nerd Font 16")

;(let ((faces '(mode-line
;               mode-line-buffer-id
;               mode-line-emphasis
;               mode-line-highlight
;               mode-line-inactive)))
;     (mapc
;      (lambda (face) (set-face-attribute face nil :font "Miosevka Nerd Font 12"))
;      faces))

;(set-face-attribute 'mode-line nil  :height 100)
;(set-face-attribute 'header-line nil  :height 200)

; https://stackoverflow.com/questions/2813273/emacs-23-2-opens-a-new-window-for-each-compile-error-warning-navigated-to/3921613#3921613
; this is needed, so that compilation buffer does not open error file with split
(setq split-width-threshold nil)

; https://stackoverflow.com/questions/9725015/how-do-i-make-the-compilation-window-in-emacs-to-always-be-a-certain-size/9726633#9726633
(setq compilation-window-height 12)
;(add-hook 'compilation-mode-hook (lambda () (text-scale-decrease 2)))

;(when (eq system-type 'darwin)
;  (when window-system
;    ;; does not work when in early-init.el?
;    (add-hook 'compilation-mode-hook (lambda () (text-scale-decrease 2)))
;  )
;)

; https://emacs.stackexchange.com/questions/7583/transiently-adjust-text-size-in-mode-line-and-minibuffer/7584#7584
; worse code https://stackoverflow.com/questions/24705984/increase-decrease-font-size-in-an-emacs-frame-not-just-buffer/60641769#60641769

; not good, changes also mode-line font-size
; https://www.emacswiki.org/emacs/GlobalTextScaleMode
; text-scale works on all buffer at the same time
;(defadvice text-scale-increase (around all-buffers (arg) activate)
;  (dolist (buffer (buffer-list))
;    (with-current-buffer buffer
;      ad-do-it)))

;(set-face-attribute 'default nil :font "Iosevka Rofrol 20")
(set-face-attribute 'default nil :font "Miosevka Nerd Font 14")
(defvar face-attribute-height 180
  "Default font face height when Emacs starts.")
(set-face-attribute 'default nil :height face-attribute-height)

(defun face-attribute-height-increase ()
  (interactive)
  (setq face-attribute-height (+ face-attribute-height 2))
  (set-face-attribute 'default nil :height face-attribute-height)
  (when (bound-and-true-p dashboard-mode)
    (dashboard-refresh-buffer)))

(defun face-attribute-height-decrease ()
  (interactive)
  (setq face-attribute-height (- face-attribute-height 2))
  (set-face-attribute 'default nil :height face-attribute-height)
  (when (bound-and-true-p dashboard-mode)
    (dashboard-refresh-buffer)))

(define-key global-map (kbd "M-<wheel-up>") 'face-attribute-height-increase)
(define-key global-map (kbd "M-<wheel-down>") 'face-attribute-height-decrease)

;; Optional: Reset line spacing and margins
(setq-default line-spacing 0)
(setq-default left-margin-width 0)
(setq-default right-margin-width 0)


(defvar my-buffer-font-size 200
  "Default font size for text-mode, prog-mode, and dashboard-mode buffers.")

(defun my-update-buffer-font-size ()
  "Update the font size for the current buffer if it's in `prog-mode`, `text-mode`, or `dashboard-mode`."
  (when (derived-mode-p 'conf-mode 'text-mode 'prog-mode 'dashboard-mode)
      (setq buffer-face-mode-face `(:height, my-buffer-font-size))
      (buffer-face-mode 1)))

(defun my-increase-font-size ()
  "Increase font size in `prog-mode`, `text-mode`, and `dashboard-mode` buffers."
  (interactive)
  (setq my-buffer-font-size (+ my-buffer-font-size 1))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (my-update-buffer-font-size))))

(defun my-decrease-font-size ()
  "Decrease font size in `prog-mode`, `text-mode`, and `dashboard-mode` buffers."
  (interactive)
  (setq my-buffer-font-size (- my-buffer-font-size 1))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (my-update-buffer-font-size))))

(global-set-key (kbd "<C-wheel-up>") 'my-increase-font-size)
(global-set-key (kbd "<C-wheel-down>") 'my-decrease-font-size)

;; Ensure all relevant buffers get the correct font size
(add-hook 'after-change-major-mode-hook 'my-update-buffer-font-size)

(provide 'rf-font)
