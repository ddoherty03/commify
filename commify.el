;;; commify.el --- Toggle grouping commas in numbers

;; Copyright (C) 2015 Daniel E. Doherty

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Daniel E. Doherty <ded-commify@ddoherty.net>
;; Version: 1.2.1
;; Package-Requires: ((s "1.9.0"))
;; Keywords: convenience, editing, numbers, grouping, commas
;; URL: https://github.com/ddoherty03/commify

;;; Commentary:

;; This package provides a simple command to toggle a number under the cursor
;; between having grouped digits and not.  For example, if the buffer is as
;; shown with the cursor at the '*':
;;
;; Travel expense is 4654254654*
;;
;; invoking commify-toggle will change the buffer to:
;;
;; Travel expense is 4,654,254,654*
;;
;; Calling commify-toggle again removes the commas.  The cursor can also be
;; anywhere in the number or immediately before or after the number.
;; commify-toggle works on floating or scientific numbers as well, but it only
;; ever affects the digits before the decimal point.  Afterwards, the cursor
;; will be placed immediately after the affected number.
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

(require 's)

;; Customize options.

(defgroup commify nil
  "Toggle insertion of commas in numbers in buffer."
  :group 'convenience)

(defcustom commify-group-char ","
  "Character to use for separating groups of digits in decimal numbers."
  :type 'string
  :group 'commify)

(defcustom commify-decimal-char "."
  "Character recognized as the decimal point for decimal numbers."
  :type 'string
  :group 'commify)

(defcustom commify-group-size 3
  "Number of digits in each group for decimal numbers."
  :type 'integer
  :group 'commify)

(defcustom commify-alt-group-char "_"
  "Character to use for separating groups of non-decimal digits.

You can enable the commify to commify forms of numbers other than
the default decimal numbers.  For example, you can set up commify
to add grouping to hexadecimal, octal, or binary numbers by
defining appropriate regular expressions for
`commify-alt-prefix-re' and `commify-alt-suffix-re' and a
character range for `commify-alt-digits' to recognize a number in
a non-decimal base.  If you do so, this string is used to
separate the digits into groups of `commify-alt-group-size'."
  :type 'string
  :group 'commify)

(defcustom commify-alt-prefix-re "0[xXoObB]"
  "Regular expression prefix required before a number in a non-decimal base."
  :type 'regexp
  :group 'commify)

(defcustom commify-alt-digits "0-9A-Fa-f"
  "Character class of valid digits in a number in a non-decimal base."
  :type 'regexp
  :group 'commify)

(defcustom commify-alt-suffix-re ""
  "Regular expression suffux required after a number in a non-decimal base."
  :type 'regexp
  :group 'commify)

(defcustom commify-alt-group-size 4
  "Number of digits in each group for non-decimal base numbers."
  :type 'integer
  :group 'commify)

;; Utility functions

(defun commify--number-re ()
  "Regular expression of a valid number string.

A valid number has a mandatory whole number part, which it
captures as the second group.  The number may contain the
`commify-group-char' in the whole number part and uses
`commify-decimal-char' as the separator between the whole and
fractional part of the number.  A leading sign, `+' or `-' is
optional, as is a trailing exponent introduced by `e' or `E'.

The matched sub-parts are:
  1. the optional sign,
  2. the whole number part,
  3. the optional fractional part, including the decimal point, and
  4. the optional exponent part."
  (let ((sign "\\([-+]\\)?")
        (whole (concat "\\([0-9" (regexp-quote commify-group-char) "]+\\)"))
        (frac (concat "\\(" (regexp-quote commify-decimal-char) "[0-9]+\\)?"))
        (exp "\\([eE][-+0-9]+\\)?"))
    (concat sign whole frac exp)))

(defun commify--alt-number-re ()
  "Regular expression of a valid number string in a non-decimal base.

A valid number in a non-decimal base has an optional sign, a
mandatory prefix, a mandatory whole number part composed of the
valid digits and the grouping character, which it captures as the
third group, and a mandatory suffix, which may be empty.

The matched sub-parts are:
  1. the optional sign
  2. the pre-fix,
  3. the whole number part, and
  4. the suffix."

  (let ((sign (concat "\\([-+]\\)?"))
        (pre (concat "\\(" commify-alt-prefix-re "\\)"))
        (whole (concat "\\([" (regexp-quote commify-alt-group-char)
                       (regexp-quote commify-alt-digits) "]+\\)"))
        (suffix (concat "\\(" (regexp-quote commify-alt-suffix-re) "\\)")))
    (concat sign pre whole suffix)))

(defun commify--exception-p (str)
  "Should the STR be excluded from commify?"
  (or (commify--date-p str)
      (commify--indentifier-p str)
      (commify--zero-filled-p str)))

(defun commify--date-p (str)
  "Is STR part of a date?"
  (save-match-data
    (or (string-match-p "\\(?:19\\|20\\)[[:digit:]]\\{2\\}[-/]" str)
        (string-match-p "[-/]\\(?:19\\|20\\)[[:digit:]]\\{2\\}" str))))

(defun commify--indentifier-p (str)
  "Is STR part of an identifier?"
  (save-match-data
    (string-match-p "^[A-Za-z]\\s_" str)))

(defun commify--zero-filled-p (str)
  "Is STR a zero-padded number?"
  (save-match-data
    (string-match-p "^0[^xobXOB]" str)))

(defun commify--current-nonblank ()
  "Return the string from the buffer of all non-blank characters around the cursor"

  (save-excursion
    (skip-chars-backward "^[:blank:]"
                         (max (point-min) (line-beginning-position)))
    (let ((beg (point)))
      (skip-chars-forward "^[:blank:]" (min (point-max) (line-end-position)))
      (buffer-substring beg (point)))))

(defun commify--move-to-next-nonblank ()
  "Move the cursor to the beginning of the next run of non-blank characters after the cursor"

  (if (< (point) (point-max))
      (progn
        (skip-chars-forward "^\n[:blank:]" (point-max))
        (skip-chars-forward "\n[:blank:]" (point-max)))
    0))

(defun commify--commas (n  &optional group-char group-size valid-digits)
  "For an integer string N, insert GROUP-CHAR between groups of GROUP-SIZE VALID-DIGITS."
  (unless group-char (setq group-char commify-group-char))
  (unless group-size (setq group-size commify-group-size))
  (unless valid-digits (setq valid-digits "0-9"))
  (if (< group-size 1)
      n
    (let ((num nil)
          (grp-re nil)
          (rpl-str nil))
      ;; reverse the string so we can insert the commas left-to-right
      (setq num (s-reverse n))
      ;; form the re to look for groups of group-size digits, e.g. "[0-9]\{3\}"
      (setq grp-re (concat "[" valid-digits "]" "\\{" (format "%s" group-size) "\\}"))
      ;; form the replacement, e.g., "\&,"
      (setq rpl-str (concat "\\&" group-char))
      ;; do the replacement in the reversed string
      (setq num (replace-regexp-in-string grp-re rpl-str num))
      ;; now chop off any trailing group-char and re-reverse the string.
      (s-reverse (s-chop-suffix group-char num)))))

(defun commify--uncommas (n &optional group-char)
  "For an integer string N, remove all instances of GROUP-CHAR."
  (unless group-char (setq group-char commify-group-char))
  (s-replace-all `((,group-char . "")) n))

;; Commands

;;;###autoload
(defun commify-toggle-at-point ()
  "Toggle insertion or deletion of grouping characters in the number around point."
  (interactive)
  (unless (commify--exception-p (commify--current-nonblank))
    (save-excursion
      ;; find the beginning of the non-blank run of text the cursor is in or
      ;; after, limited to the beginning of the line or the beginning of buffer.
      (skip-chars-backward "^[:blank:]"
                           (max (point-min) (line-beginning-position)))
      (cond
       ;; a hex, octal, or binary number, usually
       ((looking-at (commify--alt-number-re))
        (let ((num (match-string 3))
              (num-beg (match-beginning 3))
              (num-end (match-end 3)))
            (delete-region num-beg num-end)
            ;; We may have point at a +/- sign, skip over
            (goto-char num-beg)
            (if (s-contains? commify-alt-group-char num)
                (insert-before-markers (commify--uncommas num commify-alt-group-char))
              (insert-before-markers (commify--commas
                       num commify-alt-group-char commify-alt-group-size
                       commify-alt-digits)))
            (goto-char num-end)))
       ;; a decimal number
       ((looking-at (commify--number-re))
        (let ((num (match-string 2))
              (num-beg (match-beginning 2))
              (num-end (match-end 2)))
            (delete-region num-beg num-end)
            ;; We may have point at a +/- sign, skip over
            (goto-char num-beg)
            (if (s-contains? commify-group-char num)
                (insert-before-markers (commify--uncommas num commify-group-char))
              (insert-before-markers (commify--commas
                       num commify-group-char commify-group-size)))
            (goto-char num-end)))))))

;;;###autoload
(defun commify-toggle-on-region (beg end)
  "Toggle insertion or deletion of numeric grouping characters.
Do so for all numbers in the region between BEG and END."
  (interactive "r")
  (save-excursion
    (let ((deactivate-mark)
          (end-mark (copy-marker end)))
      (goto-char beg)
      (commify-toggle-at-point)
      (while (and (> (commify--move-to-next-nonblank) 0)
                  (<= (point) end-mark))
        (commify-toggle-at-point)))))

;;;###autoload
(defun commify-toggle ()
  "Toggle commas at point or on the region from BEG to END."
  (interactive)
  (if (use-region-p)
      (commify-toggle-on-region (region-beginning) (region-end))
    (commify-toggle-at-point)))


(provide 'commify)
;;; commify.el ends here
