emacs-org-transform-tree-table
==============================

Transform an org-mode outline and its properties to a table format
(org-table, CSV).

This makes it possible to have an outline with properties and work
with it in column view. Then you can transform the outline to a table
to share with others (export to CSV and open in Excel).

More about column view:

* http://orgmode.org/worg/org-tutorials/org-column-view-tutorial.html
* http://orgmode.org/worg/org-tutorials/org-column-view-tutorial.html


## Usage

    ;; To an org table
    M-x org-transform-tree/org-table-buffer-from-outline

    ;; To CSV (or rather, tab-separated value)
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


## Tiny example

This outline:

    * Pages
      :PROPERTIES:
      :COLUMNS:  %30ITEM %10Access %10Cost
      :END:
    ** Products
       :PROPERTIES:
       :Access:   All
       :END:
    *** Free Widget
        :PROPERTIES:
        :Access:   All
        :END:
    *** Paid Thingy
        :PROPERTIES:
        :Access:   Paid
        :Cost:     30
        :END:

Transforms into:

    | Heading         | Access | Cost | COLUMNS                   |
    | * Pages         |        |      | %30ITEM %10Access %10Cost |
    | ** Products     | All    |      |                           |
    | *** Free Widget | All    |      |                           |
    | *** Paid Thingy | Paid   |   30 |                           |



## Examples

Let's say you have an outline like this.

    * Pages
    ** About
    ** Products
    *** Free Gizmo
    *** Free Widget
    *** Paid Thingy
    ** Courses
    *** Introduction
    *** Advanced
    *** Super Advanced

with the two columns degined under "* Pages"

  :COLUMNS:  %30ITEM %10Access %10Cost

And some property values set on various headings:

    * Pages
      :PROPERTIES:
      :COLUMNS:  %30ITEM %10Access %10Cost
      :Access_ALL: All Paid
      :END:
    ** About
       :PROPERTIES:
       :Access:   All
       :END:
    ** Products
       :PROPERTIES:
       :Access:   All
       :END:
    *** Free Gizmo
        :PROPERTIES:
        :Access:   All
        :END:
    *** Free Widget
        :PROPERTIES:
        :Access:   All
        :END:
    *** Paid Thingy
        :PROPERTIES:
        :Access:   Paid
        :Cost:     30
        :END:
    ** Courses
       :PROPERTIES:
       :Access:   All
       :END:
    *** Introduction
        :PROPERTIES:
        :Access:   All
        :END:
    *** Advanced
        :PROPERTIES:
        :Access:   Paid
        :Cost:     10
        :END:
    *** Super Advanced
        :PROPERTIES:
        :Access:   Paid
        :Cost:     100
        :END:


This outline transformed to an org-table looks like this:

    | Heading            | Access | Cost | COLUMNS                   | Access_ALL |
    | * Pages            |        |      | %30ITEM %10Access %10Cost | All Paid   |
    | ** About           | All    |      |                           |            |
    | ** Products        | All    |      |                           |            |
    | *** Free Gizmo     | All    |      |                           |            |
    | *** Free Widget    | All    |      |                           |            |
    | *** Paid Thingy    | Paid   |   30 |                           |            |
    | ** Courses         | All    |      |                           |            |
    | *** Introduction   | All    |      |                           |            |
    | *** Advanced       | Paid   |   10 |                           |            |
    | *** Super Advanced | Paid   |  100 |                           |            |

It can also be written to a tab-separated value buffer, which can be
opened in e.g. Excel.


