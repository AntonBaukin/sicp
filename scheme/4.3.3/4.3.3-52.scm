(define (log . args) (for-each display args) (newline))

(define eval-amb-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.7/eval-impl-registry.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.3.1/eval-impl-amb.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.3.1/eval-impl-forms.scm"
  "../4.3.3/4.3.3-52-if-fail.scm"
  "../4.3.1/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "../4.3.1/eval-amb.scm")

(eval-basic
 (define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items)))
 )

 (define (test . items)
  (if-fail
   (let ((x (an-element-of items)))
    (require (even? x))
    x
   )
   'all-odd
  )
 )

 (global test)
)

; First sample from the task:
(assert-equal? '(all-odd) (eval-amb-results (test 1 3 5)))

; Second sample from the task:
(assert-equal? '(8) (eval-amb-results (test 1 3 5 8)))
