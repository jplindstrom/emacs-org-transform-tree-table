emacs-org-transform-tree-table
==============================

Transform an org-mode outline with properties to a table format
(org-table, CSV), or the other way around.

This makes it possible to have an outline with properties and work
with it in column view.

Then you can transform the outline to a table to share with others
(export to CSV and open in Excel), and back again.

More about column view:

* http://orgmode.org/worg/org-tutorials/org-column-view-tutorial.html
* http://orgmode.org/manual/Column-view.html


More about Excel:

* People who aren't Emacs users tend to use it a lot.


## Usage

### From org tree, to table

    ;; Org outline to an org table
    M-x org-transform-tree/org-table-buffer-from-outline

    ;; Org outline to CSV (or rather, tab-separated value)
    M-x org-transform-tree/csv-table-buffer-from-outline

If the region is active, convert that part of the
tree. Otherwise, if point is on an org heading, convert that
heading and its subtree. Otherwise convert the buffer.

In the resulting table, row one is the column titles. The rest of
the rows are property values.

Column one is the outline heading, and the rest are the
properties in the order they first appear in the buffer.

However, all special properties (e.g. 'COLUMNS', '*_ALL') are
placed after all the user properties (i.e. whatever properties
the user has added to capture information).

Text content under a heading is also transformed and put in the first
column.

Special values that can't be represented in an org table are escaped:

    |                   ==> \vert{}
    first leading space ==> non-breaking space (C-x 8 SPC)


### From table, to org tree

    ;; From an org table to an org outline
    M-x org-transform-table/org-tree-buffer-from-org-table

    ;; From CSV (tab separated) to an org outline
    M-x org-transform-table/org-tree-buffer-from-csv

When converting from an org table, point must be on a table.

When converting CSV, convert the buffer.

Values escaped from any tree->table transformation are unescaped (see
above).



## Tiny example

This outline:

    * Pages
      :PROPERTIES:
      :COLUMNS:  %30ITEM %10Login %10Access %10Cost
      :END:
    ** About
       :PROPERTIES:
       :Login:    No
       :Access:   All
       :END:
    ** Products
       :PROPERTIES:
       :COLUMNS: %30ITEM %10Color
       :Access:   All
       :Login:    No
       :END:
    *** Free Widget
        :PROPERTIES:
        :Access:   All
        :Login:    Yes
        :Color:    Green
        :END:

        This one is:
        - Awesome
        - Green
    *** Paid Thingy
        :PROPERTIES:
        :Access:   Paid
        :Cost:     30
        :Login:    Yes
        :Color:    Blue
        :END:

Transforms into:

    | Heading          | Login | Access | Color | Cost | COLUMNS                            |
    | * Pages          |       |        |       |      | %30ITEM %10Login %10Access %10Cost |
    | ** About         | No    | All    |       |      |                                    |
    | ** Products      | No    | All    |       |      | %30ITEM %10Color                   |
    | *** Free Widget  | Yes   | All    | Green |      |                                    |
    |     This one is: |       |        |       |      |                                    |
    |     - Awesome    |       |        |       |      |                                    |
    |     - Green      |       |        |       |      |                                    |
    | *** Paid Thingy  | Yes   | Paid   | Blue  |   30 |                                    |

Note that:

* The special property COLUMNS are out on the right, to be out of the
  way when the table is being edited in e.g. Excel or Open Office.

* The transformation is only 99% round-trip safe since there might be
  some reordering of properties taking place.

* It's possible to have many COLUMNS declarations for different parts
  of the tree. Some of them might be repeated at a lower level. Useful
  techniques to keep them in sync:
  * Multiple cursors
  * M-x iedit
  * Search and replace

* Each line of text under a heading turns into a row.

* The indentation / whitespace in the text has a leading
  non-breaking-space to keep the layout inside the table.

* The outline can also be written to a tab-separated value buffer,
  which can be opened in e.g. Excel.


## Installation

### MELPA

Install org-transform-tree-table using MELPA like any other module.


### Manually

Clone the repo into somewhere in the load-path.

    git clone https://github.com/jplindstrom/emacs-org-transform-tree-table.git

and initialize with:

   (require 'org-transform-tree-table)


