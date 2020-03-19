; We customize basic evaluator with Hugo's version of eval:
(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/4.1.2-2.a-eval.scm"  ;<— Hugo's evaluator
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.1/eval-impl-set.scm"
 )
)

(include "../4.1.1/eval-basic.scm")


; *** ERROR IN eval-impl, "4.1.1-2.a-eval.scm"@18.5 --
;  Unbound variable name define
(eval-basic (define x 3))
