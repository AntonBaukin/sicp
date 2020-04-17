(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  ; Here we implement unbind:
  "../4.1.3/4.1.3-13-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.3/4.1.3-13-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "../4.1.2/eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

(assert-eq? 'abc
 (eval-basic
  (define (test x)
   (define (nest x)
    (debug log "Nest «x» initial = " x)
    ; On this sample, see comments in «4.1.3-13-forms.scm».
    (unbind x)
    (debug log "Nest «x» unbound = " x)
    x ;<— resulting value
   )
   
   (debug log "Test «x» = " x)
   (nest 'def)
  )

  (test 'abc)
 )
)