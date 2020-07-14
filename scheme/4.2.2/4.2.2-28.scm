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
  ; In this file we alter «apply-basic-lazy()»:
  "../4.2.2/4.2.2-28-lazy.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.7/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "eval-lazy.scm")
(eval-basic (debug on))

(eval-basic
 ; Task 28 asks us not to resolve a procedure before
 ; invoking it. In this case only native functions
 ; may be called? Let's check this matter...

 (debug log "In task 28 we do not force a procedure...")

 (debug log "We able to call primitives, such as (+ 1 2 3) = " (+ 1 2 3))

 (define (plus a b c) (+ a b c))

 ; Defined function also works as it's not thunked...
 (debug log "What about define function? (plus 1 2 3) = " (plus 1 2 3))

 ; But what about function arguments?
 (define (call f a b c) (f a b c))

 (debug log "The problem is with function arguments, (call plus 1 2 3)")
 (debug log "——— Expected error:\n")
 (call plus 1 2 3)

 ; Just comment out line "../4.2.2/4.2.2-28-lazy.scm" and run again...
)
