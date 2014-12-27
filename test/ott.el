;;; test/ott.el --- Test for org-transform-tree-table.el

;; Copyright © 2014 Johan Linsdtrom
;;
;; Author: Johan Lindstrom <buzzwordninja not_this_bit@googlemail.com>
;; URL: https://github.com/jplindstrom/emacs-org-transform-tree-table
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.


(require 'ert)


(defun ott-data-file (file)
  "Return file name of test data FILE"
  (concat default-directory "data/" file))

(defun ott-data-file-string (file)
  "Return the contents of data FILE as a string."
  (with-temp-buffer
    (insert-file-contents (ott-data-file file))
    (buffer-string)))

(defmacro with-ott-org-file (file &rest body)
  `(with-temp-buffer
     (insert-file-literally (ott-data-file ,file))
     (org-mode)
     ,@body))


(defun ott-should-string= (got expected &rest description)
  (when description (message "%s" description))
  (or
   (string= got expected)
   (message "string= failed: got (\n%s\n), but expected (\n%s\n)" got expected))

  (should ;; nil to empty string
   (string= got expected )
   )
  )


(ert-deftest ott--org-table--unescape ()
  (should ;; nil to empty string
   (string= (ott/org-table/unescape-value nil) ""))

  (should ;; Normal, unescaped
   (string= (ott/org-table/unescape-value "Hello there") "Hello there"))

  (should ;; Normal, multiple times
   (string=
    (ott/org-table/unescape-value "A pipe: \\vert{}, and again \\vert{}")
    "A pipe: |, and again |"
    ))

  (should ;; Without {}
   (string=
    (ott/org-table/unescape-value "A normal \\vert, and more \\vert here and at end \\vert")
    "A normal |, and more | here and at end |"))

  (should ;; Without {}, as part of longer word
   (string=
    (ott/org-table/unescape-value "As part of word \\vertical")
    "As part of word \\vertical"))
)

(ert-deftest ott--org-tree--heading-text ()
  "Correct heading-text"

  ;;;; Setup
  (with-ott-org-file
   "tree2.org"


   (goto-char (point-min))
   (search-forward "First A with property")
   (ott-should-string=
    (ott/org-tree/heading-text)
    "
   Blank line before
   and after

"
    "Blank lines")


   (goto-char (point-min))
   (search-forward "Second")
   (ott-should-string=
    (ott/org-tree/heading-text)
    "This makes it possible to have an outline with properties and work with it in column view. Then you can transform the outline
to a table to share with others (export to CSV and open in Excel).

  - with a list, which is indented
  - and another
    - sub-item
"
    "With a drawer")


   (goto-char (point-min))
   (ott-should-string=
    (ott/org-tree/heading-text)
    ""
    "No text")

   (search-forward "Second C")
   (ott-should-string=
    (ott/org-tree/heading-text)
    "   One level of indentation
"
    "Indented text")

   )
  )

(ert-deftest ott--transform-tree-to-org-table--buffer ()
  "Check that on org-mode buffer transforms ok to an org-table"

  ;;;; Setup
  (with-ott-org-file
   "tree1.org"

   ;; Go to place outside of tree
   (goto-char (point-min))

   ;;;; Run
   (with-current-buffer
       (org-transform-tree/org-table-buffer-from-outline)
     ;;;; Test
     ;; Extraction did the right thing
     (ott-should-string=
      (buffer-substring-no-properties (point-min) (point-max))
      (ott-data-file-string "expected-tree1-buffer--table.org")
      "Transformed text from expected-tree1-buffer--table.org matches")
     )
   )
  )


(ert-deftest ott--transform-tree-to-org-table--subtree ()
  "Check that on org-mode tree transforms ok to an org-table"

  ;;;; Setup
  (with-ott-org-file
   "tree1.org"

   ;; Go to place on heading
   (goto-char (point-min))
   (search-forward "** Second B")

   ;;;; Run
   (with-current-buffer
       (org-transform-tree/org-table-buffer-from-outline)
     ;;;; Test
     ;; Extraction did the right thing
     (should
      (string=
       (buffer-substring-no-properties (point-min) (point-max))
       (ott-data-file-string "expected-tree1-heading--table.org")))
     )
   )
  )


(ert-deftest ott--transform-tree-to-org-table--region ()
  "Check that on org-mode region transforms ok to an org-table"

  ;;;; Setup
  (with-ott-org-file
   "tree1.org"

   ;; Go to place on heading
   (goto-char (point-min))
   (search-forward "*** A 2")
   (beginning-of-line)
   (set-mark (point))
   (search-forward "*** B 1 -")
   (end-of-line)

   ;;;; Run
   (with-current-buffer
       (org-transform-tree/org-table-buffer-from-outline)
     ;;;; Test
     ;; Extraction did the right thing
     (should
      (string=
       (buffer-substring-no-properties (point-min) (point-max))
       (ott-data-file-string "expected-tree1-region--table.org")))
     )
   )
  )



(ert-deftest ott--transform-org-table-to-tree ()
  "Check that on org-table transforms ok to an org tree"

  ;;;; Setup
  (with-ott-org-file
   "org-table1.org"

   ;; Go to place in table
   (goto-char (point-min))

   ;;;; Run
   (with-current-buffer
       (org-transform-table/org-tree-buffer-from-org-table)
     ;;;; Test
     ;; Extraction did the right thing
     (should
      (string=
       (buffer-substring-no-properties (point-min) (point-max))
       (ott-data-file-string "expected-org-table1--tree.org")))
     )
   )
  )

(ert-deftest ott--transform-csv-buffer-to-tree ()
  "Check that on csv table transforms ok to an org tree"

  ;;;; Setup
  (with-ott-org-file
   "csv-table1.csv"

   ;; Go to place in table
   (goto-char (point-min))

   ;;;; Run
   (with-current-buffer
       (org-transform-table/org-tree-buffer-from-csv)
     ;;;; Test
     ;; Extraction did the right thing
     (should
      (string=
       (buffer-substring-no-properties (point-min) (point-max))
       (ott-data-file-string "expected-csv-table1--tree.org")))
     )
   )
  )

(defun ott--transform-tree-to-org-table--test-roundtrip (file)
  "Check that a roundtrip of FILE tree->table then ->tree->table
is identical"

  ;;;; Setup
  (with-ott-org-file
   file

   (goto-char (point-min))

   (let* ((table-buffer (org-transform-tree/org-table-buffer-from-outline)))
     (with-current-buffer table-buffer
       ;; Original table text
       (let ((table-text (buffer-substring-no-properties (point-min) (point-max))))
         (set-mark (point-max))
         (goto-char (point-min))
         (with-current-buffer
             (org-transform-table/org-tree-buffer-from-org-table)
           ;; as a tree
           (set-mark (point-max))
           (goto-char (point-min))
           (with-current-buffer
               ;; as a table
               (org-transform-tree/org-table-buffer-from-outline)
             (should
              (string=
               (buffer-substring-no-properties (point-min) (point-max))
               table-text)
              ))))
       )
     )))

(ert-deftest ott--transform-tree-to-org-table--roundtrip ()
  "Check that a roundtrip tree->table then ->tree->table is identical"

  ;; Simple file
  (ott--transform-tree-to-org-table--test-roundtrip "tree1.org")

  ;; with heading text, indentation
  (ott--transform-tree-to-org-table--test-roundtrip "tree2.org")

  )



;; Run tests at eval-buffer time
(ert-run-tests-interactively "^ott-")
;; (ert-run-tests-interactively "^ott--org-tree--heading-text")

;;; test/ert.el ends here
