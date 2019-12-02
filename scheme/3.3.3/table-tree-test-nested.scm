(include "tree-red-black.scm")
(include "table-tree.scm")

(define (symbol<? a b)
 (string<? (symbol->string a) (symbol->string b))
)

; Create tree table for symbols used in the test:
(define Table (make-table-tree symbol<?))

(include "table-test-nested-base.scm")
