(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")

; Run this test before «4.2.2-30-a-ben.scm».

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
  ; Here we rewrite «analyze-sequence» as Pablo wants:
  "../4.2.2/4.2.2-30-seq.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "eval-lazy.scm")
(eval-basic (debug on))

(eval-basic
 (define (for-each proc items)
  (if (null? items) 'done
   (begin
    (proc (car items))
    (for-each proc (cdr items))
   )
  )
 )

 (debug log "\nResult of for-each call:\n"
  (for-each
   (lambda (x)
    (newline)
    (display x)
   )
   (list 57 321 88)
  )
 )
)

; The following text is printed:

; <— empty line is here
; 57
; 321
; 88
; :> Result of for-each call:
; done


;
; Let's clarify what Pablo is afraid of.
; Let's take sample from «4.2.2-27.scm»...
;

; Here we expect «count» to be 1 instead of 3,
; because call to «b» is thunked and not resolved.
;
; Note that «a» and «b» do differ, and we may see
; what one is actually invoked — it's «a».
;
(assert-eq? 1
 (eval-basic
  (define count 0)

  (define (a x)
   (set! count (+ count 1)) ;<— side effect
   x
  )

  (define (b x)
   (set! count (+ count 2)) ;<— side effect
   x
  )

  (define w (a (b 10)))

  count ;<— is 1
 )
)

; Now we replace the same sample with «begin» form.
;
; Pablo's version resolves (a (b 10)), former «w».
; It firces side-effect of count increment or «b»,
; and the expected result is correct.
;
; So,
;
(assert-eq? 3
 (eval-basic
  (define count 0)

  (define (a x)
   (set! count (+ count 1)) ;<— side effect
   x
  )

  (define (b x)
   (set! count (+ count 2)) ;<— side effect
   x
  )

  (begin
   (a (b 10)) ;<— is forced
   count      ;<— is 3
  )
 )
)

; Pablo gives his own clarification sample. Lets check it.
(eval-basic
 (define (p1 x)
  (set! x (cons x '(2)))
  x
 )

 (define (p2 x)
  (define (p e)
   e
   x
  )

  (p (set! x (cons x '(2))))
 )

 (debug log "Pablo runs his sample:")
 (debug log " (p1 1) = " (p1 1))
 (debug log " (p2 1) = " (p2 1))
)

; Now run «4.2.2-30-a-ben.scm»...
