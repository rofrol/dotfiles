;;; -*- lexical-binding: t; -*-


;;; q will kill dired buffer, not just hide it
;; https://emacs.stackexchange.com/questions/54578/rebind-q-key-in-emacs-dired-to-the-command-kill-this-buffer#comment85263_54579
(with-eval-after-load 'dired
  (define-key dired-mode-map "q" 'kill-this-buffer)
  (define-key dired-mode-map (kbd "<left>") 'dired-jump)
  (define-key dired-mode-map (kbd "<right>") 'dired-find-directory)
  (define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)
  ;; https://stackoverflow.com/questions/20580152/jump-to-a-certain-file-in-emacs-dired/20617005#20617005
  (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
  (define-key dired-mode-map (kbd "C-M-s") 'dired-isearch-filenames-regexp)
  (define-key dired-mode-map "c" 'find-file)
  (require 'dired-x))

;; TODO: Unused
;; https://emacs.stackexchange.com/a/204/19475
(defun kill-dired-buffers ()
     (interactive)
     (mapc (lambda (buffer) 
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
             (kill-buffer buffer))) 
         (buffer-list)))

;; TODO: Unused
(defun dired-find-directory ()
  "A `dired-find-file' which only works on directories."
  (interactive)
  (let ((find-file-run-dired t)
        (file (dired-get-file-for-visit)))
    (when (file-directory-p file)
      (find-file file))))

;; ;; https://groups.google.com/g/gnu.emacs.help/c/rLIO54wA5Ow
;; (defun dired-show-only (regexp)
;;   (interactive "sFiles to show (regexp): ")
;;   (dired-mark-files-regexp regexp)
;;   (dired-toggle-marks)
;;   (dired-do-kill-lines))

(rf/require 'dired-narrow)

(with-eval-after-load 'dired-x
  (setq dired-omit-files
    (concat dired-omit-files "\\|^\\..+$\\|\\.pdf$\\|\\.tex$|\\.DS_Store"))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

(provide 'rf-dired)