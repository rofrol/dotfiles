;;; -*- lexical-binding: t; -*-

(defun my/treesit-generate-parser (&rest args)
  "If there is no parser.c, run tree-sitter generate."
  (when (and (equal "parser.c" (car (last args)))
             (not (file-exists-p (expand-file-name "parser.c")))
             ;; on macOS: brew install tree-sitter-cli
             (executable-find "tree-sitter"))
    (let ((default-directory (file-name-parent-directory default-directory)))
      (message "Generating parser.c with tree-sitter...")
      (treesit--call-process-signal
       (executable-find "tree-sitter") nil t nil "generate"))))

(advice-add 'treesit--call-process-signal :before #'my/treesit-generate-parser)

(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (odin  "https://github.com/tree-sitter-grammars/tree-sitter-odin")))

(dolist (lang (mapcar #'car treesit-language-source-alist))
  (unless (treesit-language-available-p lang)
    (message "Installing tree-sitter grammar for %s..." lang)
    (treesit-install-language-grammar lang)))

(use-package odin-ts-mode
  :vc (:url "https://github.com/Sampie159/odin-ts-mode.git")
  :mode "\\.odin\\'"
  :hook (odin-ts-mode . eglot-ensure)
  :config
  (add-hook 'odin-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'eglot-format-buffer nil t))))

(provide 'rf-tree-sitter)
