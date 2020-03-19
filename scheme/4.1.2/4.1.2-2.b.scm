; We customize basic evaluator with Hugo's version of eval:
(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/4.1.2-2.a-eval.scm"  ;<— the same Hugo's evaluator
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.2/4.1.2-2.b-set.scm"   ;<— Hugo's specials
 )
)

(include "../4.1.1/eval-basic.scm")
(include "../3.3.2/assert.scm")


; So, here are a lot of calls...
(assert-eq? 24
 (eval-basic
   (define (factorial n)
    (if (call = n 1) 1
     (call * n (call factorial (call - n 1)))
    )
   )

   (call factorial 4)
 )
)