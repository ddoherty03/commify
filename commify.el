(require 's)

;;; Code:

(defvar cfy/group-char ",")
(defvar cfy/decimal-char ".")
(defvar cfy/group-size 3)

(defun cfy/commas (n  &optional group-char group-size)
  "Return a string from integer N that inserts COMMA-CHAR between groups of GROUP-SIZE digits"
  (unless group-char (setq group-char cfy/group-char))
  (unless group-size (setq group-size cfy/group-size))
  (let ((num nil)
        (grp-re nil)
        (rpl-str nil))
    (setq num (s-reverse (format "%s" n)))
    (setq grp-re (concat "[0-9]\\{" (format "%s" group-size) "\\}"))
    (setq rpl-str (concat "\\&" group-char))
    (s-reverse (replace-regexp-in-string grp-re rpl-str num))))

(defun cfy/toggle-commas ()
  (interactive)
  "Insert or delete grouping characters from the number around point"
  (save-excursion
    (skip-chars-backward (concat cfy/decimal-char cfy/group-char "0-9e+-"))
    (when (looking-at "[-+]")
      (skip-chars-forward "-+"))
    (when (looking-at "[0-9]")
      (let ((beg-num (point))
            (num nil))
        (skip-chars-forward (concat cfy/group-char "0-9"))
        (setq num (delete-and-extract-region beg-num (point)))
        (if (s-contains? cfy/group-char num)
            (insert (s-replace-all `((,cfy/group-char . "")) num))
          (insert (cfy/commas (string-to-number num))))
        (goto-char beg-num))))
  (skip-chars-forward (concat cfy/decimal-char cfy/group-char "0-9e+-")))

(provide 'commify)
;;; commify.el ends here
