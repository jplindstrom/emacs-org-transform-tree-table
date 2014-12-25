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
     (should
      (string=
       (buffer-substring-no-properties (point-min) (point-max))
       (ott-data-file-string "expected-tree1-buffer--table.org")))
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

(ert-deftest ott--transform-tree-to-org-table--roundtrip ()
  "Check that a roundtrip tree->table then ->tree->table is identical"

  ;;;; Setup
  (with-ott-org-file
   "tree1.org"

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

;; Run tests at eval-buffer time
(ert-run-tests-interactively "^ott-")

;;; test/ert.el ends here
