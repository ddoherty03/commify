;;; commify.el --- Toggle grouping commas in numbers in buffer

;; Copyright (C) 2015 Daniel E. Doherty

;; Author: Daniel E. Doherty <ded-commify@ddoherty.net>
;; Version: 1.0
;; Package-Requires: ((s "1.0"))
;; Keywords: numbers, grouping, commas
;; URL: http://github.com/ddoherty03/commify

;;; Commentary:

;; This package provides a simple command to toggle a number under the cursor
;; between having grouped digits and not.  For example, if the buffer is as
;; shown with the cursor at the '*':
;;
;; Travel expense is 4654254654*
;;
;; invoking commify/toggle will change the buffer to:
;;
;; Travel expense is 4,654,254,654*
;;
;; Calling commify-toggle again removes the commas.  The cursor can also be
;; anywhere in the number or immediately before or after the number.
;; commify/toggle works on floating or scientific numbers as well, but in only
;; ever affect the digits before the decimal point.
;;
;; You can configure these variables:
;;   - commify-group-char (default ",") to the char used for grouping
;;   - commify-group-size (default 3) to number of digits per group
;;   - commify-decimal-char (default ".") to the char used as a decimal point.
;;
;; Bind the main function to a convenient key in you init.el file:
;;
;;    (key-chord-define-global ",," 'commify-toggle)

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
