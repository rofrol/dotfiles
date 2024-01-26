;;; -*- lexical-binding: t; -*-

(rf/require 'clojure-mode)
(rf/require 'cider)
(add-hook 'clojure-mode-hook #'cider-mode)

(provide 'rf-clojure)
