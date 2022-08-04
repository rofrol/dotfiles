;;; -*- lexical-binding: t; -*-

;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
;; https://www.reddit.com/r/emacs/comments/44jwh3/comment/czte0lx/
(defun rofrol/delete-compile-window-if-successful (buffer string)
 "Delete a compilation window if succeeded without warnings "
 (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-at-time 3 nil
      (lambda ()
        (select-window (get-buffer-window (get-buffer-create "*compilation*")))
        (delete-window)))))
(add-hook 'compilation-finish-functions 'rofrol/delete-compile-window-if-successful)

(provide 'rf-compile)