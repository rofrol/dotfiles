;;; -*- lexical-binding: t; -*-

;; https://ebzzry.com/en/emacs-pairs/
;; https://explog.in/dot/emacs/config.html

(rf/require 'smartparens)
(require 'smartparens-config)

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(defmacro def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ;; error: Eager macro-expansion failure: (void-function loop)
     ;; I have changed from loop to cl-loop
     ;; >Nearly everywhere loop is used should be replaced with cl-loop
     ;; https://www.reddit.com/r/emacs/comments/tjt2ie/comment/i1m67om/
     ;; >imho evil-surround + visual mode is more powerful and flexible: make a visual selection, activate evil-surround, press the paren you want and it will take care of inserting the surrounding pair, optionally with spaces, and so on.
     ;;  https://emacs.stackexchange.com/questions/28891/configure-smartparens-wrapping-to-work-in-evil-visual-state/29293#29293
     ;; >It is built in, kind of, select the region and do M-( or M-x insert-parentheses
     ;; https://www.reddit.com/r/emacs/comments/8siime/comment/e13c8fu/
     ;; (electric-pair-mode 1) similar
     ,@(cl-loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

(with-eval-after-load
    'show-smartparens-global-mode t
    (define-key smartparens-mode-map (kbd "C-M-a" 'sp-beginning-of-sexp))
    (define-key smartparens-mode-map (kbd "C-M-e" 'sp-end-of-sexp))
    
    (define-key smartparens-mode-map (kbd "C-<down>" 'sp-down-sexp))
    (define-key smartparens-mode-map (kbd "C-<up>" 'sp-up-sexp))
    (define-key smartparens-mode-map (kbd "M-<down>" 'sp-backward-down-sexp))
    (define-key smartparens-mode-map (kbd "M-<up>" 'sp-backward-up-sexp))
    
    
    
    (define-key smartparens-mode-map (kbd "C-M-f" 'sp-forward-sexp))
    (define-key smartparens-mode-map (kbd "C-M-b" 'sp-backward-sexp))
    
    (define-key smartparens-mode-map (kbd "C-M-n" 'sp-next-sexp))
    (define-key smartparens-mode-map (kbd "C-M-p" 'sp-previous-sexp))
    
    (define-key smartparens-mode-map (kbd "C-S-f" 'sp-forward-symbol))
    (define-key smartparens-mode-map (kbd "C-S-b" 'sp-backward-symbol))
    
    
    
    (define-key smartparens-mode-map (kbd "C-<right>" 'sp-forward-slurp-sexp))
    (define-key smartparens-mode-map (kbd "M-<right>" 'sp-forward-barf-sexp))
    (define-key smartparens-mode-map (kbd "C-<left>" 'sp-backward-slurp-sexp))
    (define-key smartparens-mode-map (kbd "M-<left>" 'sp-backward-barf-sexp))
    
    (define-key smartparens-mode-map (kbd "C-M-t" 'sp-transpose-sexp))
    (define-key smartparens-mode-map (kbd "C-M-k" 'sp-kill-sexp))
    (define-key smartparens-mode-map (kbd "C-k" 'sp-kill-hybrid-sexp))
    (define-key smartparens-mode-map (kbd "M-k" 'sp-backward-kill-sexp))
    (define-key smartparens-mode-map (kbd "C-M-w" 'sp-copy-sexp))
    (define-key smartparens-mode-map (kbd "C-M-d" 'delete-sexp))
    
    (define-key smartparens-mode-map (kbd "M-<backspace>" 'backward-kill-word))
    (define-key smartparens-mode-map (kbd "C-<backspace>" 'sp-backward-kill-word))
    (define-key smartparens-mode-map (kbd [remap sp-backward-kill-word] . backward-kill-word))
    
    (define-key smartparens-mode-map (kbd "M-[" 'sp-backward-unwrap-sexp))
    (define-key smartparens-mode-map (kbd "M-]" 'sp-unwrap-sexp))
    
    (define-key smartparens-mode-map (kbd "C-x C-t" 'sp-transpose-hybrid-sexp))

    (define-key smartparens-mode-map (kbd "C-c (" 'wrap-with-parens))
    (define-key smartparens-mode-map (kbd "C-c [" 'wrap-with-brackets))
    (define-key smartparens-mode-map (kbd "C-c {" 'wrap-with-braces))
    (define-key smartparens-mode-map (kbd "C-c '" 'wrap-with-single-quotes))
    (define-key smartparens-mode-map (kbd "C-c \"" 'wrap-with-double-quotes))
    (define-key smartparens-mode-map (kbd "C-c _" 'wrap-with-underscores))
    (define-key smartparens-mode-map (kbd "C-c `" 'wrap-with-back-quotes)))

;; does not work inside `(with-eval-after-load`
;; https://github.com/oshima/dotfiles/blob/d933fad1d49758e75baf3c60ce127047cc3ef71d/.emacs.d/inits/20_smartparens.el#L8
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

;; after RET indent and add }
;; https://github.com/jaguirresza/.emacs.d/blob/a5f21a38bb69a61f7aa7485dc9d7c4cc85132158/settings/smart-parens.el#L22
;; (when (fboundp 'sp-local-pair)
;;   (sp-local-pair '(js-mode typescript-mode) "{" nil
;;                  :when '(("RET"))
;;                  :post-handlers '(:add rf/curly-braces-newline-handle)
;;                  :actions '(insert)))

(defun rf/curly-braces-newline-handle (id action context)
  (when (eq action 'insert)
    (save-excursion (newline-and-indent))
    (indent-according-to-mode)))

;; insert {} and after RET indent
;; https://github.com/ogdenwebb/snug-emacs/blob/5cfa136d5ba67d8dd20f4e963ef1be9757366ae1/use/use-sp.el#L14
(sp-with-modes '(js-mode typescript-mode)
  (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))

;; should I enable it?
;; (setq sp-show-pair-delay 0)
;; (setq sp-show-pair-from-inside t)
;; (setq sp-cancel-autoskip-on-backward-movement nil)

(provide 'rf-smartparens)