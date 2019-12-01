(include "table.scm")
(include "../2.3.3/curry.scm")
(include "../3.3.2/assert.scm")


; This implementation takes minimal subset of operations
; implemented in «table.scm» with functions taking
; table as the first argument. Here we dispatch them.
; Complete showcase of them is in «table-test.scm».
(define (make-simple-table same-key?)
 ; Operations set for the given keys comparator:
 (define Table (make-table same-key?))

 ; Create table instance:
 (define table ((table-op-make Table)))

 ; Curried operations of interest:
 (define table-lookup (curry (table-op-lookup Table) table))
 (define table-add (curry (table-op-add Table) table))

 (lambda (method)
  (cond
   ((eq? method 'lookup) table-lookup)
   ((eq? method 'insert!) table-add)
   (else (error "Unknown table method" method))
  )
 )
)


(define table (make-simple-table eq?))

((table 'insert!) 1 'a)
((table 'insert!) 2 'b)
((table 'insert!) 3 'c)
((table 'insert!) 4 'd)

(assert-eq? 1 ((table 'lookup) 'a))
(assert-eq? 2 ((table 'lookup) 'b))
(assert-eq? 3 ((table 'lookup) 'c))
(assert-eq? 4 ((table 'lookup) 'd))

; Returns void on absent:
(assert-eq? void ((table 'lookup) 'e))
