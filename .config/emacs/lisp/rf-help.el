;;; -*- lexical-binding: t; -*-

; https://emacs.stackexchange.com/questions/3356/select-help-window-buffer-after-c-h-f
; https://emacs.stackexchange.com/questions/58717/how-to-close-help-buffer-without-moving-to-it/58718#58718
(setq help-window-select t)

; https://emacs.stackexchange.com/questions/58717/how-to-close-help-buffer-without-moving-to-it/70407#70407
(defun reset-help-window ()
  "Restore help window to previous state"
  (let ((help-window nil) (windows (window-list)))
    (dotimes (i (length (window-list)))
      (when (equal (buffer-name) "*Help*")
    (setf help-window (nth i windows)))
      (other-window 1))
    (if help-window
    (quit-restore-window help-window)
      (print "No Help window"))))

(add-to-list 'display-buffer-alist
             '("^\\*Help\\*$"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (window-height . 12)))


(provide 'rf-help)
