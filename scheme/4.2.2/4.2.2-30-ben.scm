(define (log . args) (for-each display args) (newline))

; NOTE! Run this test after «4.2.2-30-a-pablo.scm».

(include "../3.3.2/assert.scm")
(include "eval-lazy.scm")
(eval-basic (debug on))

; Here we run test with our default «analyze-sequence»
; that Ben stands for...
(eval-basic
 (define (for-each proc items)
  (if (null? items) 'done
   (begin
    (proc (car items))
    (for-each proc (cdr items))
   )
  )
 )

 (debug log "\n:> Result of for-each call:\n"
  (for-each
   (lambda (x)
    (newline)
    (display x)
   )
   (list 57 321 88)
  )
 )
)

; Exactly the same text is printed as in Pablo's version:

; <— empty line is here
; 57
; 321
; 88
; :> Result of for-each call:
; done

; So, are Pablo's concerns superfluous?
; Let's check...

; Pablo's version gives count 3, Ben's — 1.
; Unexpected difference in almost the same lazy evaluator!
;
; This may drive us crasy! Side-effects hell...
;
(assert-eq? 1 ;<— not 3
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
   (a (b 10)) ;<— is not forced
   count      ;<— is still 1!
  )
 )
)

;
; Ben's sample «for-each» gives the same result as Pablo's.
; Why? We expect Ben's sample to be shallow...
;
; Note that is our special sample in expression «(a (b 10))»
; function «a» is actually invoked passing it «(b 10)» as
; a thunk being not invoked — chesk 1 Vs 2 increments.
;
; The same stands for «(proc (car items))»: it's invoked,
; and thunked «(car items)» is then forced in «if» test
; predicate. But returned «x», being thunk of «(b 10)»,
; is not forced in «a».
;

; If we want to dwell with side-effects in our lazily
; evaluated functional language, we have to agree
; with Pablo, and fix our implementation...

; Lets check Pablo's sample.
(eval-basic
 (define (p1 x)
  ; This primary function is invoked in both versions:
  (set! x (cons x '(2)))
  x
 )

 (define (p2 x)
  (define (p e)
   e ; <— this argument is not forced, and side effect on «x»
   x ;    is not resolved in our (and Ben's) evaluator.
  )

  (p (set! x (cons x '(2))))
 )

 (debug log "Ben runs Pablo's sample:")
 (debug log " (p1 1) = " (p1 1))
 (debug log " (p2 1) = " (p2 1))
)

; Pablo runs his sample:
;  (p1 1) = (1 2)
;  (p2 1) = (1 2)
;
; Ben runs Pablo's sample:
;  (p1 1) = (1 2)
;  (p2 1) = 1
