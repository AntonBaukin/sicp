; Files «table-tree-test.scm» and «table-tree-test-nested.scm»
; contain generalized massive random tests to ensure the main
; features of balanced red-black trees do work.
;
; This file samples the same cases as 24 and 25 tasks.
; You may check that actual implementation of balanced
; tree goes far beyond «mere» task 3.26 os SICP...
;

(include "../2.3.3/curry.scm")
(include "../3.3.2/assert.scm")
(include "../3.1/accumulator.scm")
(include "tree-red-black.scm")
(include "table-tree.scm")


(define (log . args) (for-each display args) (newline))

(define (symbol<? a b)
 (string<? (symbol->string a) (symbol->string b))
)

; Create tree table for symbols used in the test:
(define Table (make-table-tree symbol<?))
(define table? (table-op-table? Table))
(define add (table-op-add Table))
(define lookup (table-op-lookup Table))

; Make sample instance:
(define table ((table-op-make Table)))

(add table 1 'a)
(add table 2 'b)
(add table 3 'c)
(add table 4 'd)

(assert-eq? 1 (lookup table 'a))
(assert-eq? 2 (lookup table 'b))
(assert-eq? 3 (lookup table 'c))
(assert-eq? 4 (lookup table 'd))

; Returns void on absent:
(assert-eq? void (lookup table 'e))

; Create two nested tables 'e and 'f:
(add table 5 'e 'f 'g)
(assert-eq? 5 (lookup table 'e 'f 'g))

(add table 6 'e 'h)
(assert-eq? 5 (lookup table 'e 'f 'g))
(assert-eq? 6 (lookup table 'e 'h))


(define (table->string table)
 (define S (make-concatenator ", "))

 ((table-op-iterate Table)
  table
  (lambda (key value)
   (S (string-append
    (symbol->string key)
    ": "
    (if (table? value)
     (table->string value)
     (number->string value)
    )
   ))
   void ;<— return it not to overwrite!
  )
 )

 ; Call (S) returns accumulated string:
 (string-append "{ " (S) " }")
)

(log "Resulting table (see the source asserts):\n"
 (table->string table)
)
