;;; -*- lexical-binding: t; -*-

;(rf/require 'zig-mode)
;(rf/require 'zig-ts-mode)
(setq major-mode-remap-alist
 '((zig-mode . zig-ts-mode)))

;; https://emacs.stackexchange.com/questions/4108/execute-external-script-upon-save-when-in-a-certain-mode/4115#4115
(defun zig-test-buffer-hook ()
  "Run zig test on save"
  (when (eq major-mode 'zig-mode)
    (zig-test-buffer)))

;(add-hook 'after-save-hook #'zig-test-buffer-hook)

(defun rf/watchexec-zig-build ()
  "Run watchexec -r zig build"
  (interactive)
  (when (eq major-mode 'zig-mode)
    (compile )))

(use-package zig-ts-mode
  ;:quelpa (zig-mode :host github :repo "nanzhong/zig-mode" :branch "tree-sitter")
  :quelpa (zig-ts-mode :fetcher codeberg :repo "meow_king/zig-ts-mode")
  :demand t
  :mode (("\\.zig\\'" . zig-ts-mode)
         ("\\.zon\\'" . zig-ts-mode))
  :init
  (add-to-list 'treesit-language-source-alist
               '(zig "https://github.com/maxxnino/tree-sitter-zig"))
  :hook (zig-ts-mode . (lambda ()
                         (setq treesit-font-lock-level 4))))

(defun rf/zig-insert-braces ()
  "Insert braces for Zig function declarations with proper formatting."
  (interactive)
  (progn
    (message "Entered rf/zig-insert-braces")
    (when (and (derived-mode-p 'zig-ts-mode)
               (eq last-command-event ?{))
      (message "Entered when")
      (let* ((node (treesit-node-at (point)))
             (parent (treesit-node-parent node))
             (builtin-type-node (treesit-node-child parent 4)))
        (message "Node: %s" node)
        (message "Node type: %s" (if node (treesit-node-type node) "nil"))
        (message "Parent: %s" parent)
        (message "Parent type: %s" (if parent (treesit-node-type parent) "nil"))
        (message "Builtin type: %s" builtin-type-node)
        (when (and parent
                   (string= (treesit-node-type parent) "function_declaration")
                   builtin-type-node
                   (string= (treesit-node-type builtin-type-node) "builtin_type"))
          (message "Entered when 2")
          ;; Delete the auto-inserted opening brace
          (delete-char -1)
          ;; Insert formatted braces with a space before
          (insert " {\n\n}")
          ;; Position cursor on empty line between braces
          (forward-line -1)
          (indent-according-to-mode)
          ;; Indent the closing brace
          (forward-line 1)
          (indent-according-to-mode)
          ;; Move back to the empty line and indent cursor position
          (forward-line -1)
          (indent-for-tab-command))))))

(define-key zig-ts-mode-map "{" 'rf/zig-insert-braces)

(defun insert-newline-and-close-brace ()
  "Inserts a newline followed by a closing brace."
  (interactive)
  (insert "{\n}"))

;(define-key prog-mode-map "{" 'insert-newline-and-close-brace)


(provide 'rf-zig)
