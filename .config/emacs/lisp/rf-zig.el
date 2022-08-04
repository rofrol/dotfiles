;;; -*- lexical-binding: t; -*-

(rf/require 'zig-mode)

;; https://emacs.stackexchange.com/questions/4108/execute-external-script-upon-save-when-in-a-certain-mode/4115#4115
(defun zig-test-buffer-hook ()
  "Run zig test on save"
  (when (eq major-mode 'zig-mode)
    (zig-test-buffer)))

(add-hook 'after-save-hook #'zig-test-buffer-hook)

(provide 'rf-zig)