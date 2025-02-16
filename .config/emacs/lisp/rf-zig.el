;;; -*- lexical-binding: t; -*-

;(rf/require 'zig-mode)
;(rf/require 'zig-ts-mode)
(setq major-mode-remap-alist
 '((zig-mode . zig-ts-mode)))

(defun my/enable-treesit-explorer-mode ()
  "Enable `treesit-explorer-mode` and automatically confirm language prompt."
  (interactive)
  (let ((read-answer-short t))
    (cl-letf (((symbol-function 'completing-read) 
               (lambda (&rest _) "zig")))
      (treesit-explore-mode))))

(add-hook 'zig-ts-mode-hook #'my/enable-treesit-explorer-mode)

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
  :ensure t
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

;for cases:
;const Elephant = struct {
;    letter: u8,
;    tail: ?*Elephant = null,
;    visited: bool = false,
;
;    // New Elephant methods!
;    fn visit4(self: *Elephant) void 
;    pub fn getTail(self: *Elephant) *Elephant {
;        return self.tail.?; // Remember, this means "orelse unreachable"
;    }
;    fn visit3(self: *Elephant) 
;    pub fn hasTail(self: *Elephant) bool {
;        return (self.tail != null);
;    }
;
;    fn visit2(self: *Elephant) void 
;// std.debug.log
;
;    pub fn print(self: *Elephant) void {
;        // Prints elephant letter and [v]isited
;        const v: u8 = if (self.visited) 'v' else ' ';
;        std.debug.print("{u}{u} ", .{ self.letter, v });
;    }
;};

(defun rf/zig-insert-braces ()
  "Insert braces for Zig function declarations with proper formatting."
  (interactive)
  (when (derived-mode-p 'zig-ts-mode)
    (let* ((node (treesit-node-at (point)))
           (parent (treesit-node-parent node))
           (sibling-error-node (when node
                                 (let ((sibling (treesit-node-next-sibling node)))
                                 (catch 'found
                                   (dotimes (i 10)  ; Check first 10 siblings
                                     (message "sibling: %s" (treesit-node-type sibling))
                                     (if (and sibling
                                              (string= (treesit-node-type sibling) "ERROR"))
                                       (throw 'found sibling)
                                       (setq sibling (treesit-node-next-sibling sibling))))))))
           (ancestor-is-func-decl (when node
                    (let ((prev node)
                          (ancestor (treesit-node-parent node)))
                    (catch 'found
                      (dotimes (i 10)  ; Check first 10 ancestors
                        (message "ancestor %s" (treesit-node-type ancestor))
                        (if (and ancestor
                                 (string= (treesit-node-type ancestor) "function_declaration"))
                          (throw 'found ancestor)
                          (setq ancestor (treesit-node-parent ancestor))))))))
           (should-insert-brace (and parent
               ancestor-is-func-decl
               (or (not (string= (treesit-node-type node) "block"))
                   (and
                    (string= (treesit-node-type node) "identifier")
                    (string= (treesit-node-type parent) "ERROR"))
                    sibling-error-node))))
      (if should-insert-brace
          ;; Function declaration case
          (progn
            (message "Before end-of-line, point: %d" (point))
            (end-of-line)
            (message "After end-of-line, point: %d" (point))
            (insert " ")
            (message "After space, point: %d" (point))
            ;; Try inserting newlines before the braces
            (sp-insert-pair "{")
            (message "After sp-insert-pair, point: %d" (point))
            (newline)
            (newline)
            (indent-according-to-mode)
            (forward-line -1)
            (message "After forward-line -1, point: %d" (point))
            (indent-according-to-mode)
            (message "After indent, point: %d" (point)))
        ;; Normal case - let smartparens handle it
        (sp-insert-pair "{")))))

(define-key zig-ts-mode-map "{" 'rf/zig-insert-braces)

;(defun insert-newline-and-close-brace ()
;  "Inserts a newline followed by a closing brace."
;  (interactive)
;  (insert "{\n}"))

;(define-key prog-mode-map "{" 'insert-newline-and-close-brace)


(provide 'rf-zig)
