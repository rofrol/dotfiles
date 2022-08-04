;;; -*- lexical-binding: t; -*-

;; https://wikemacs.org/wiki/Expand_region
(rf/require 'expand-region)
;; = to expand, - to contract-region, 0 to reset
(global-set-key (kbd "C-=") 'er/expand-region)
;; (global-set-key (kbd "C-;") 'er/expand-region)
;; (setq expand-region-contract-fast-key "'")

(provide 'rf-expand-region)