; Define «Table» implementation before include.

(include "../3.3.2/assert.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define table-make    (table-op-make Table))
(define table?        (table-op-table? Table))
(define table-size    (table-op-size Table))
(define table-add     (table-op-add Table))
(define table-remove  (table-op-remove Table))
(define table-lookup  (table-op-lookup Table))
(define table-clear   (table-op-clear Table))
(define table-iterate (table-op-iterate Table))

(define table (table-make))

(define (table->string table)
 (define S (make-concatenator ", "))

 (table-iterate table
  (lambda (key value)
   (S (string-append
    (symbol->string key)
    ": "
    (number->string value)
   ))
   void ;<— return it not to overwrite!
  )
 )

 ; Call (S) returns accumulated string:
 (string-append "{ " (S) " }")
)


(assert-test table table?)
(assert-test-not (cons 'table '()) table?)
(assert-eq? 0 (table-size table))

(table-add table 1 'a)
(assert-eq? 1 (table-size table))
(assert-eq? 1 (table-lookup table 'a))
(assert-eq? void (table-lookup table '+))

(table-add table 2 '+)
(assert-eq? 2 (table-size table))
(assert-eq? 1 (table-lookup table 'a))
(assert-eq? 2 (table-lookup table '+))
(assert-eq? void (table-lookup table '!))

(table-add table 3 '!)
(assert-eq? 3 (table-size table))
(assert-eq? 1 (table-lookup table 'a))
(assert-eq? 2 (table-lookup table '+))
(assert-eq? 3 (table-lookup table '!))

(table-add table 4 'abc)
(assert-eq? 4 (table-size table))
(assert-eq? 4 (table-lookup table 'abc))

(assert-eq? table (table-clear table))
(assert-eq? 0 (table-size table))

(table-add table 1 'a)
(table-add table 2 'b)
(table-add table 3 'c)
(table-add table 4 'd)
(table-add table 5 'e)

(log "a-e table " (table->string table))
(assert-true?
 (let ((s (table->string table)))
  (or
   (equal? s "{ e: 5, d: 4, c: 3, b: 2, a: 1 }")
   (equal? s "{ a: 1, b: 2, c: 3, d: 4, e: 5 }")
  )
 )
)

; Square all existing items of the table.
; This is done without any overhead (re-inserts).
(table-iterate table (lambda (key value) (square value)))
(log "square values " (table->string table))

; Overwrite two values:
(assert-eq? 9 (table-add table 3 'c))
(assert-eq? 3 (table-lookup table 'c))
(assert-eq? 25 (table-add table 5 'e))
(assert-eq? 5 (table-lookup table 'e))
(log "reset c and e " (table->string table))

(assert-eq? 16 (table-remove table 'd))
(assert-eq? 4 (table-size table))
(assert-eq? void (table-lookup table 'd))
(log "removed d " (table->string table))
