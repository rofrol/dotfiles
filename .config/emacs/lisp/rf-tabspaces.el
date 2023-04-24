;;; -*- lexical-binding: t; -*-

(rf/require 'tabspaces)
(with-eval-after-load 'tabspaces
  (setq-default tabspaces-use-filtered-buffers-as-default t
                tabspaces-default-tab "Default"
                tabspaces-remove-to-default t
                tabspaces-include-buffers '("*scratch*"))
  (tabspaces-mode))

(provide 'rf-tabspaces)