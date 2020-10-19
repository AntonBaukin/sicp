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
  "../4.3.3/4.3.3-54-require.scm"
  "../4.3.1/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "../4.3.1/eval-amb.scm")

;
; We name this form as capital «REQUIRE».
;

; Basic, but working test:
(assert-equal? '(0 2 4 6 8)
 (eval-amb-results
  (define (test)
   (define n (amb 0 1 2 3 4 5 6 7 8 9))
   (REQUIRE (even? n))
   n
  )
  (test)
 )
)

; Test from «eval-amb-test-amb.scm»:
(assert-equal?
 (list void 3 2 1)
 (eval-amb-list
  (define (an-element-of items)
   (REQUIRE (not (null? items)))
   (amb (car items) (an-element-of (cdr items)))
  )

  (an-element-of '(1 2 3))
 )
)
