;;; -*- lexical-binding: t; -*-

;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/

;; https://github.com/andersmurphy/.emacs.d/blob/7d19d4c93e8734a58fee0a7791fdfd5ff553689b/init.el#L773
;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
;; https://github.com/andersmurphy/andersmurphy-blog/blob/90884606ee01f878d80c8ff757cc7ba42ffdceec/resources/posts/2020-08-20-emacs-setting-up-apheleia-to-use-zprint.md#L27
;; (rf/require 'apheleia)
;; (with-eval-after-load 'apheleia
;;   (apheleia-global-mode +1)
;;   (setf (alist-get 'prettier apheleia-formatters)
;;      '("yarn" "prettier"
;;         ;; "--trailing-comma"  "es5"
;;         ;; "--bracket-spacing" "true"
;;         ;; "--single-quote"    "true"
;;         ;; "--semi"            "false"
;;         ;; "--print-width"     "100"
;;         filepath)))

;; with https://github.com/radian-software/apheleia there was noticeable delay after save
;; >If you're looking for a decent autosave experience with js/ts/json/yaml/ruby I highly recommend disabling doom format mode and using prettier-mode instead 
;; https://www.reddit.com/r/DoomEmacs/comments/mfr0ed/comment/gsp9yxa/
;; https://github.com/jscheid/prettier.el#differences-from-prettier-emacs
;; other general formatters
;; https://github.com/purcell/emacs-reformatter
;; https://github.com/lassik/emacs-format-all-the-code
;; [Merge apheleia and/or format-all-the-code and/or reformatter · Issue #170 · lassik/emacs-format-all-the-code](https://github.com/lassik/emacs-format-all-the-code/issues/170)
;; https://emacs.stackexchange.com/questions/34135/cant-get-prettier-js-working/38011#38011
;; https://200ok.ch/posts/2019-03-02_autoformatting_source_code.html
(rf/require 'prettier)
(add-hook 'after-init-hook #'global-prettier-mode)

;; M-: (prettier--parsers) RET
;; M-: (prettier-enabled-parsers) RET
;; https://github.com/jscheid/prettier.el/issues/101#issuecomment-1134475617

;; needed to install prettier with `npm i -g prettier` and `M-x prettier-restart`.

(provide 'rf-prettier)