;
; Define «Table» implementation before including this file.
;
(include "../3.3.2/assert.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define table-make     (table-op-make Table))
(define table?         (table-op-table? Table))
(define table-size     (table-op-size Table))
(define table-add      (table-op-add Table))
(define table-remove   (table-op-remove Table))
(define table-lookup   (table-op-lookup Table))
(define table-clear    (table-op-clear Table))
(define table-iterate  (table-op-iterate Table))
(define table-iterator (table-op-iterator Table))

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

(table-add table 5 'e)
(table-add table 4 'd)
(table-add table 3 'c)
(table-add table 2 'b)
(table-add table 1 'a)

(define (check-str str)
 (assert-equal? str (table->string table))
)

(log "a-to-e table " (table->string table))
(check-str "{ a: 1, b: 2, c: 3, d: 4, e: 5 }")

(define it (table-iterator table))

(define (check-it key value)
 (define kvs (it)) ; ((key value) . set))

 (assert-eq? key (caar kvs))
 (assert-eq? value (cadar kvs))
 (assert-true? (procedure? (cdr kvs)))
)

(define (set-it key value)
 (define kvs (it)) ; ((key value) . set))

 (assert-eq? key (caar kvs))
 (assert-true? (procedure? (cdr kvs)))
 ((cdr kvs) (car kvs) value)
)

(check-it 'a 1)
(check-it 'b 2)
(check-it 'c 3)
(check-it 'd 4)
(check-it 'e 5)
(assert-true? (null? (it)))

; Square all existing items of the table.
; This is done without any overhead (re-inserts).
(table-iterate table (lambda (key value) (square value)))
(log "square values " (table->string table))
(check-str "{ a: 1, b: 4, c: 9, d: 16, e: 25 }")

; Overwrite two values:
(assert-eq? 9 (table-add table 3 'c))
(assert-eq? 3 (table-lookup table 'c))
(assert-eq? 25 (table-add table 5 'e))
(assert-eq? 5 (table-lookup table 'e))
(log "reset c and e " (table->string table))
(check-str "{ a: 1, b: 4, c: 3, d: 16, e: 5 }")

(assert-eq? 16 (table-remove table 'd))
(assert-eq? 4 (table-size table))
(assert-eq? void (table-lookup table 'd))
(log "removed d " (table->string table))
(check-str "{ a: 1, b: 4, c: 3, e: 5 }")

(set! it (table-iterator table))

(set-it 'a 2)
(set-it 'b 8)
(set-it 'c 6)
(set-it 'e 10)
(assert-true? (null? (it)))
(log "doubled " (table->string table))
(check-str "{ a: 2, b: 8, c: 6, e: 10 }")
