;;; 5009-region-tests.el --- Tests for 5009-region.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Julian Hoch <julianhoch@web.de>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for 5009-region.el

;;; Code:

(require 'ert)
(require '5009-region)

;; Test din5009-char
(ert-deftest test-din5009-char ()
  "Test `din5009-char' function."
  (should (string= (din5009-char ?a) "Anton"))
  (should (string= (din5009-char ?A) "Anton"))
  (should (string= (din5009-char ?ä) "Ärger"))
  (should (string= (din5009-char ?.) "Punkt"))
  (should (string= (din5009-char ?x) "Xanthippe")))

;; Test de5009-char
(ert-deftest test-de5009-char ()
  "Test `de5009-char' function."
  (should (string= (de5009-char "Anton") "a"))
  (should (string= (de5009-char "Punkt") "."))
  (should (string= (de5009-char "Charlotte") "ch"))
  (should (string= (de5009-char "Schule") "sch"))
  (should (string= (de5009-char "Komma") ","))
  (should (string= (de5009-char "Unknown") "Unknown")))

;; Test din5009-string
(ert-deftest test-din5009-string ()
  "Test `din5009-string' function."
  ;; Basic conversion
  (should (string= (din5009-string "abc") "Anton-Berta-Cäsar"))
  
  ;; Spaces
  (should (string= (din5009-string "a b") "Anton Berta"))
  
  ;; Punctuation
  (should (string= (din5009-string "a.") "Anton-Punkt"))
  (should (string= (din5009-string "a,b") "Anton-Komma-Berta"))
  
  ;; Special digraphs
  (should (string= (din5009-string "sch") "Schule"))
  (should (string= (din5009-string "ch") "Charlotte"))
  
  ;; Mixed case
  (should (string= (din5009-string "AbC") "Anton-Berta-Cäsar"))
  
  ;; Unknown characters
  (should (string= (din5009-string "a#b") "Anton-#-Berta"))
  
  ;; Complex example
  (should (string= (din5009-string "Hallo, Welt!") 
                   "Heinrich-Anton-Ludwig-Ludwig-Otto-Komma Wilhelm-Emil-Ludwig-Theodor-Ausrufezeichen")))

;; Test de5009-word
(ert-deftest test-de5009-word ()
  "Test `de5009-word' function."
  (should (string= (de5009-word "Anton") "a"))
  (should (string= (de5009-word "Punkt") "."))
  (should (string= (de5009-word "Charlotte") "ch"))
  (should (string= (de5009-word "Unknown") "Unknown")))

;; Test de5009-string
(ert-deftest test-de5009-string ()
  "Test `de5009-string' function."
  ;; Basic conversion
  (should (string= (de5009-string "Anton-Berta-Cäsar") "abc"))
  
  ;; Spaces
  (should (string= (de5009-string "Anton Berta") "a b"))
  
  ;; Punctuation
  (should (string= (de5009-string "Anton-Punkt") "a."))
  (should (string= (de5009-string "Anton-Komma-Berta") "a,b"))
  
  ;; Special digraphs
  (should (string= (de5009-string "Schule") "sch"))
  (should (string= (de5009-string "Charlotte") "ch"))
  
  ;; Unknown words
  (should (string= (de5009-string "Anton-Unknown-Berta") "aUnknownb"))
  
  ;; Complex example
  (should (string= (de5009-string "Heinrich-Anton-Ludwig-Ludwig-Otto-Komma Wilhelm-Emil-Ludwig-Theodor-Ausrufezeichen") 
                   "hallo, welt!")))

;; End-to-end tests
(ert-deftest test-din5009-roundtrip ()
  "Test round-trip conversion (text -> DIN 5009 -> text)."
  (let ((original-texts '("Hello, World!"
                          "This is a test."
                          "Special chars: äöüß"
                          "Digraphs: ch and sch"
                          "1234567890")))
    (dolist (text original-texts)
      (should (string= (de5009-string (din5009-string text)) 
                       (downcase text))))))

(provide '5009-region-tests)
;;; 5009-region-tests.el ends here
