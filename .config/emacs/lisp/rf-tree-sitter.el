;;; -*- lexical-binding: t; -*-

(setq treesit-language-source-alist '())

(defvar rf/treesit-node-info ""
  "Stores current tree-sitter node information for display in mode line.")

(defun rf/get-node-position-info (node cursor-pos)
  "Get position info relative to the node."
  (when node
    (let* ((node-start (treesit-node-start node))
           (node-end (treesit-node-end node))
           (current-node (treesit-node-at cursor-pos))
           (prev-node (treesit-node-prev-sibling current-node))
           (next-node (treesit-node-next-sibling current-node)))
      (cond
       ((= cursor-pos node-start) " (start)")
       ((= cursor-pos node-end) " (end)")
       ((and prev-node (= cursor-pos (treesit-node-end prev-node))) " (after prev)")
       ((and next-node (= cursor-pos (treesit-node-start next-node))) " (before next)")
       (t (format " (at %s)" (or (treesit-node-type current-node) "boundary")))))))

(defun rf/update-treesit-node-info ()
  "Update the tree-sitter node information when cursor moves."
  (when (and (treesit-available-p)
             (derived-mode-p 'prog-mode))
    (let* ((current-node (treesit-node-at (point)))
           (parent-node (treesit-node-parent current-node))
           (prev-sibling (treesit-node-prev-sibling current-node))
           (pos-info (rf/get-node-position-info parent-node (point))))
      (setq rf/treesit-node-info
            (format " [Node: %s, Parent: %s, Prev: %s%s]"
                    (when current-node (treesit-node-type current-node))
                    (when parent-node (treesit-node-type parent-node))
                    (if prev-sibling 
                        (treesit-node-type prev-sibling)
                      "nil")
                    pos-info))
      (force-mode-line-update))))


;;; Add to mode-line-format
;(unless (member 'rf/treesit-node-info mode-line-format)
;  (setq-default mode-line-format
;                (append mode-line-format '(rf/treesit-node-info))))
;
;;; Add cursor movement hook
;(add-hook 'post-command-hook #'rf/update-treesit-node-info)

(provide 'rf-tree-sitter)
