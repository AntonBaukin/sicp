(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.7/eval-impl-registry.scm"
  "../4.2.2/eval-impl-thunk.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.7/eval-impl-analyze.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.2.2/eval-impl-lazy.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.7/eval-impl-forms.scm"
  ; Alters lazy behaviour of the evaluator:
  "../4.2.2/eval-impl-lazy-off.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-lazy.scm")
(include "../4.1.6/eval-assert.scm")

(eval-basic (debug on))

; By default our evaluator is not lazy:
(assert-error
 (lambda ()
  (eval-basic
   (define (xtry a b)
    (if (= a 0) 1 b)
   )
   (xtry 0 (/ 1 0))
  )
 )
 void
)

; Note that we do not alter Scheme syntax as task 31 asks:
; ((/ 1 0) lazy) — instead we write (lazy (/ 1 0)), where
; «lazy» creates thunk and works like «delay» form.
(assert-eq? 1
 (eval-basic
  (define (xtry a b)
   (if (= a 0) 1 b)
  )
  (xtry 0 (lazy (/ 1 0)))
 )
)

; Sample from «4.2.2-30-ben.scm».
(assert-eq? 1 ;<— not 3
 (eval-basic
  (define count 0)

  (define (a x)
   (set! count (+ count 1))
   x
  )

  (define (b x)
   (set! count (+ count 2))
   x
  )

  (begin
   (a (lazy (b 10)))
   count
  )
 )
)

; Now we check that our lazy is not memoized.
(eval-basic
 (define (plus a b c)
  (lambda () (+ a b c))
 )

 (define count 0)

 (define (inc x)
  (set! count (+ count 1))
  x
 )

 (define one (lazy (inc 1)))
 (define three (plus one one one))

 (assert-eq? 0 count)
 (assert-eq? 3 (three))
 (assert-eq? 3 count)
 (assert-eq? 3 (three))
 (assert-eq? 6 count)
)

; And lazy-mem is memoized.
(eval-basic
 (define (plus a b c)
  (lambda () (+ a b c))
 )

 (define count 0)

 (define (inc x)
  (set! count (+ count 1))
  x
 )

 (define one (lazy-mem (inc 1)))
 (define three (plus one one one))

 (assert-eq? 0 count)
 (assert-eq? 3 (three))
 (assert-eq? 1 count)
 (assert-eq? 3 (three))
 (assert-eq? 1 count)
)
