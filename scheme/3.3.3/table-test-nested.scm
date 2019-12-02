(include "table.scm")

; Create table with default eq? keys equality test.
(define Table (make-table eq?))

(include "table-test-nested-base.scm")
