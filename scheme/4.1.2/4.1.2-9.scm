(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

; Here we create while and for-in forms.
; They are more functional then common as they
; do return a value, not void — so, they are
; true expressions.
;
; For «register» see «4.1.2-4.a.scm».
(eval-basic (register
 while ;<— we do not to quote it

 ; Our while-form has the following format:
 ;   (while test body-exp-0[ .. body-N]).
 ;
 ; While test expression is true, body expressions
 ; are evaluated, and the result of the last one
 ; is the final result of the form when the test
 ; becomes false.
 ;
 ; On dynamic evaluation, see comments for task 4.1.2-6.
 ;
 (lambda (exp env)
  (define test (list 'eval-dynamic 'env (cadr exp)))

  (define body
   (list
    'eval-dynamic
    'env
    (if (= 1 (length (cddr exp)))
     (caddr exp)
     (append (list 'begin) (cddr exp))
    )
   )
  )

  ; Yes, we do not transform this to derived if-conditions.
  ; With the help of eval-loop we do not suffer from the
  ; luck of recursion tail optimization...
  (eval-loop (if (eval test) (eval body)))
 )

 for

 ; Our for-in loop iterates over a finite list, it is:
 ;   (for var-symbol list body-exp-0[ .. body-N]).
 ;
 ; The variable name may be used in the body expressions.
 ; The looping continues while the last expression returns
 ; not void (on void it breaks) and the list is not drained.
 ; The result of the form is the last not void value.
 ;
 ; Hint: we may easily extend this emplementation to support
 ; infinite streams with the same break condition.
 ;
 (lambda (exp env)
  ; «Compile» once, call multiple times:
  (define proc
   (eval
    (list
     'lambda
     (list (cadr exp))
     (list
      'eval-dynamic
      'env
      (if (= 1 (length (cdddr exp)))
       (caddr exp)
       (append (list 'begin) (cdddr exp))
      )
     )
    )
   )
  )

  (define seq (eval (list 'eval-dynamic 'env (caddr exp))))
  (define val '())

  (eval-loop
   (if (null? seq) void
    (begin
     ; Here the style is also procedural:
     (set! val (car seq))
     (set! seq (cdr seq))
     (apply proc (list val))
    )
   )
  )
 )
))

; Test of our while-loop:
(assert-equal? "0123456789"
 (eval-basic
  (define (digits n)
   ; Procedural loops do require local variables:
   (define i 0)
   (define s "")

   (while
    (<= i n)
    ; And assignments are inevitable:
    (set! s (string-append s (number->string i)))
    (set! i (+ i 1))
    s ;<— resulting value, accumulator
   )
  )

  (digits 9)
 )
)

; Test of our for-in-loop:
(assert-equal? 15
 (eval-basic
  (define (sum numbers)
   (define s 0)

   (for n numbers
    (set! s (+ s n))
    s ;<— resulting value, accumulator
   )
  )

  (sum '(1 2 3 4 5))
 )
)
