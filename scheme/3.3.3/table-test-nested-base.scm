; Define «Table» implementation before include.

(include "../3.3.2/assert.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define table-make    (table-op-make Table))
(define table?        (table-op-table? Table))
(define table-size    (table-op-size Table))
(define table-add     (table-op-add Table))
(define table-lookup  (table-op-lookup Table))
(define table-iterate (table-op-iterate Table))


(define (table->string table)
 (define S (make-concatenator ", "))

 (table-iterate table
  (lambda (key value)
   (S
    (string-append
     (symbol->string key)
     ": "
     (if (table? value)
      (table->string value)
      (number->string value)
     )
    )
   )
   void ;<— return it not to overwrite!
  )
 )

 ; Call (S) returns accumulated string:
 (string-append "{ " (S) " }")
)


(define table (table-make))
(table-add table 1 'a)
(assert-eq? 1 (table-size table))
(assert-eq? 1 (table-lookup table 'a))

(table-add table 2 'b 'c)
(assert-eq? 2 (table-size table))
(assert-eq? 1 (table-lookup table 'a))
(assert-test (table-lookup table 'b) table?)
(assert-eq? 1 (table-size (table-lookup table 'b)))
(assert-eq? 2 (table-lookup table 'b 'c))

(table-add table 3 'b 'd)
(assert-eq? 2 (table-size table))
(assert-eq? 1 (table-lookup table 'a))
(assert-test (table-lookup table 'b) table?)
(assert-eq? 2 (table-size (table-lookup table 'b)))
(assert-eq? 2 (table-lookup table 'b 'c))
(assert-eq? 3 (table-lookup table 'b 'd))

; Yes, create two nested intermediate tables:
(table-add table 4 'b 'e 'f 'g)
(assert-test (table-lookup table 'b) table?)
(assert-test (table-lookup table 'b 'e) table?)
(assert-test (table-lookup table 'b 'e 'f) table?)
(assert-eq? 3 (table-size (table-lookup table 'b)))
(assert-eq? 1 (table-size (table-lookup table 'b 'e)))
(assert-eq? 1 (table-size (table-lookup table 'b 'e 'f)))
(assert-eq? 4 (table-lookup table 'b 'e 'f 'g))

(table-add table 5 'b 'e 'f 'h)
(assert-eq? 3 (table-size (table-lookup table 'b)))
(assert-eq? 1 (table-size (table-lookup table 'b 'e)))
(assert-eq? 2 (table-size (table-lookup table 'b 'e 'f)))
(assert-eq? 4 (table-lookup table 'b 'e 'f 'g))
(assert-eq? 5 (table-lookup table 'b 'e 'f 'h))

(log "Resulting table " (table->string table))
