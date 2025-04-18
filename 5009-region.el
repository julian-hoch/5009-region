;;; 5009-region.el --- Convert text to DIN 5009 spelling alphabet -*- lexical-binding: t -*-

;; Copyright (C) 2025 Julian Hoch

;; Author: Julian Hoch <julianhoch@web.de>
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a function to convert text to the DIN 5009
;; spelling alphabet (German phonetic alphabet).
;;
;; Convert plain text to DIN 5009 spelling alphabet and back with
;; `din5009-region' and `de5009-region'.


;;; Code:

(defvar din5009-code
  '(("a" . "Anton")
    ("b" . "Berta")
    ("c" . "Cäsar")
    ("d" . "Dora")
    ("e" . "Emil")
    ("f" . "Friedrich")
    ("g" . "Gustav")
    ("h" . "Heinrich")
    ("i" . "Ida")
    ("j" . "Julius")
    ("k" . "Kaufmann")
    ("l" . "Ludwig")
    ("m" . "Martha")
    ("n" . "Nordpol")
    ("o" . "Otto")
    ("p" . "Paula")
    ("q" . "Quelle")
    ("r" . "Richard")
    ("s" . "Samuel")
    ("t" . "Theodor")
    ("u" . "Ulrich")
    ("v" . "Viktor")
    ("w" . "Wilhelm")
    ("x" . "Xanthippe")
    ("y" . "Ypsilon")
    ("z" . "Zacharias")
    ;; Numbers
    ("0" . "Null")
    ("1" . "Eins")
    ("2" . "Zwei")
    ("3" . "Drei")
    ("4" . "Vier")
    ("5" . "Fünf")
    ("6" . "Sechs")
    ("7" . "Sieben")
    ("8" . "Acht")
    ("9" . "Neun")
    ;; Umlaut
    ("ä" . "Ärger")
    ("ö" . "Ökonom")
    ("ü" . "Übermut")
    ("ß" . "Eszett")
    ;; Punctuation
    ("." . "Punkt")
    ("," . "Komma")
    (";" . "Semikolon")
    (":" . "Doppelpunkt")
    ("!" . "Ausrufezeichen")
    ("?" . "Fragezeichen")
    ("-" . "Bindestrich")
    ("/" . "Schrägstrich")
    ("(" . "Klammer auf")
    (")" . "Klammer zu")
    ;; Diphtong
    ("ch" . "Charlotte")
    ("sch" . "Schule"))
  "Association list of characters to their DIN 5009 representation.")

(defvar din5009-code-inverse
  (let ((inverse-list nil))
    (dolist (pair din5009-code inverse-list)
      (push (cons (cdr pair) (car pair)) inverse-list)))
  "Association list of DIN 5009 words to their character representation.
See <https://en.wikipedia.org/wiki/DIN_5009>.")

(defgroup din5009 nil
  "DIN 5009 spelling alphabet conversion."
  :group 'convenience)

(defcustom din5009-separator "-"
  "String used to separate DIN 5009 words."
  :type 'string
  :group 'din5009)

(defcustom din5009-unknown-char-string "?"
  "String used for characters that have no DIN 5009 representation."
  :type 'string
  :group 'din5009)

(defun din5009-char (char)
  "Convert CHAR to its DIN 5009 representation."
  (let* ((char-str (char-to-string (downcase char))))
    (if (= char ?\s)
        " "
      (or (cdr (assoc char-str din5009-code))
          char-str))))

(defun din5009-string (string)
  "Convert STRING to its DIN 5009 representation."
  (let ((result "")
        (i 0)
        (trimmed-string (string-trim string))) ;; Trim leading/trailing whitespace
    (while (< i (length trimmed-string))
      (let* ((char (aref trimmed-string i))
             (char-str (char-to-string (downcase char)))
             (next-char (when (< (1+ i) (length trimmed-string))
                          (downcase (aref trimmed-string (1+ i)))))
             (next-next-char (when (< (+ i 2) (length trimmed-string))
                               (downcase (aref trimmed-string (+ i 2)))))
             (digraph (when next-char
                        (concat char-str (char-to-string next-char))))
             (trigraph (when (and next-char next-next-char)
                         (concat char-str 
                                 (char-to-string next-char)
                                 (char-to-string next-next-char))))
             (din-word nil))
        
        ;; Check for special cases first
        (cond
         ;; Handle spaces specially
         ((= char ?\s)
          (setq din-word " ") ;; Use actual space for spaces
          (setq i (1+ i)))
         
         ;; Check for "sch"
         ((and (string= trigraph "sch")
               (cdr (assoc "sch" din5009-code)))
          (setq din-word (cdr (assoc "sch" din5009-code)))
          (setq i (+ i 3)))
         
         ;; Check for "ch"
         ((and (string= digraph "ch")
               (cdr (assoc "ch" din5009-code)))
          (setq din-word (cdr (assoc "ch" din5009-code)))
          (setq i (+ i 2)))
         
         ;; Regular character
         (t
          (setq din-word (or (cdr (assoc char-str din5009-code))
                             char-str))
          (setq i (1+ i))))
        
        ;; Add separator if needed
        (when (and (not (string= result ""))
                   (not (string= din-word " "))
                   (not (string= (substring result (max 0 (1- (length result)))) " ")))
          (setq result (concat result din5009-separator)))
        
        (setq result (concat result din-word))))
    result))

;;;###autoload
(defun din5009-region (beg end)
  "Convert the text in region BEG to END to DIN 5009 spelling alphabet."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (din5009-string text))))

(defun de5009-word (word)
  "Convert a DIN 5009 word to its character representation."
  (or (cdr (assoc word din5009-code-inverse))
      word))

(defun de5009-char (din-word)
  "Convert DIN 5009 word DIN-WORD to its character representation."
  (or (cdr (assoc din-word din5009-code-inverse))
      din-word))

(defun de5009-string (string)
  "Convert a DIN 5009 string to regular text."
  (let ((parts (split-string (string-trim string) " " t)) ;; Split by actual spaces first, after trimming
        (result ""))
    (dolist (part parts)
      (let ((words (split-string part din5009-separator t))
            (part-result ""))
        (dolist (word words)
          (setq part-result (concat part-result (de5009-word word))))
        (setq result (concat result part-result " "))))
    (substring result 0 (max 0 (1- (length result)))))) ;; Remove trailing space

;;;###autoload
(defun de5009-region (beg end)
  "Convert DIN 5009 spelling alphabet in region BEG to END back to regular text."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (de5009-string text))))

;;;###autoload
(defun din5009-run-tests ()
  "Run all tests for 5009-region."
  (interactive)
  (require 'ert)
  (require '5009-region-tests)
  (ert-run-tests-interactively "^test-din5009"))

(provide '5009-region)
;;; 5009-region.el ends here
