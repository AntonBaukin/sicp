(include "2.5.2-arithmetics.scm")
(include "2.5.2-tower.scm")

(define (log . args) (for-each display args) (newline))

(define I make-integer) ;<— shortcuts...
(define N make-number)

; We also directly coerce (integer —> number)
; to test with integers and float numbers.
(put-coercion '(integer number) (lambda (v)
 (if (integer? v)
  (make-number v)
  (error "Not an integer number value" v)
 )
))


; General addition functions for integers and float numbers.
(define (multi-add-i . args)
 (num-tag-set 'integer (apply + args))
)

(define (multi-add-n . args)
 (num-tag-set 'number (* 1.0 (apply + args)))
)

;(log "sum of 1 .. 5 = " (num->str (multi-add-i 1 2 3 4 5)))
;(log "sum of 1. .. 5. = " (num->str (multi-add-n 1 2 3 4 5)))

; We register sum of 3 and 4 number arguments.
; Alas, we can't register function in general way...
((apply-generic-scope-register numbers-scope)
 'add '(integer integer integer) multi-add-i
 'add '(number number number) multi-add-n
 'add '(integer integer integer integer) multi-add-i
 'add '(number number number number) multi-add-n
)

;(log "1 + 2 + 3 = " (add (I 1) (I 2) (I 3)))
;(log "1. + 2. + 3. = " (add (N 1) (N 2) (N 3)))
;(log "2 + 3 + 4 + 5 = " (add (I 2) (I 3) (I 4) (I 5)))
;(log "2. + 3. + 4. + 5. = " (add (N 2) (N 3) (N 4) (N 5)))

; The following call fails:
; Can't coerce arguments (integer number integer)
; (log "1 + 2 + 3 = " (add (I 1) (N 2) (I 3)))

(include "2.5.2-try-coerce.scm")

;(log "(i n) —> " (try-coerce '(integer number) '(1 2)))
;(log "(n i) —> " (try-coerce '(number integer) '(1 2)))
;(log "(i i n) —> " (try-coerce '(integer integer number) '(1 2 3)))
;(log "(i n i) —> " (try-coerce '(integer number integer) '(1 2 3)))
;(log "(n i i) —> " (try-coerce '(number integer integer) '(1 2 3)))

; From now we are able to make n-coercions:
(log "1 + 2. + 3 = " (add (I 1) (N 2) (I 3)))
(log "1 + 2 + 3 + 4. = " (add (I 1) (I 2) (I 3) (N 4)))


; As it's said, the coercions techinique above has a drawback:
; it is not checking for the best coercion match agains the
; operation and it's registered variations. The following
; sample demonstrates this:
;
; We create simple iterative exp function able to raise
; a number with integer power.
(define (dummy-pow n p)
 (define (iter r i)
  (if (= i 0) r
   (iter (* r n) (- i 1))
  )
 )

 (iter 1 p)
)

; (log "2 ^ 8 = " (dummy-pow 2 8))

; And we register this function with mixed arguments:
((apply-generic-scope-register numbers-scope)
 'pow '(number integer) (lambda (n p)
  (num-tag-set 'number (dummy-pow n p))
 )
)

; And define global generic call for it:
(define pow (curry num-call 'pow))

(log "Simple function of a number with integer power:")
(log "  2.0 ^ 8 = " (pow (N 2) (I 8)))
(log "but it does not work with integer argument:")
(pow (I 2) (I 8))

; To resolve this problem we have to pass the op-symbol
; into try-coerce() and make the registry lookups for
; the target function combined with raise() calls from
; the following exercises. In the case of upper sample
; we have no type int the list to coerce-to.
