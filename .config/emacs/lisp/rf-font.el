;;; -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :font "Iosevka Rofrol 20")

(defun is-in-terminal()
    (not (display-graphic-p)))

(defmacro when-term (&rest body)
  "Works just like `progn' but will only evaluate expressions in VAR when Emacs is running in a terminal else just nil."
  `(when (is-in-terminal) ,@body))

(defun font-increase(arg)
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun font-increase(arg)
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))

;(when-term
(global-set-key (kbd "C-x C-[") 'font-increase)
(global-set-key (kbd "C-x C-]") 'font-decrease)
  ;)

(provide 'rf-font)
