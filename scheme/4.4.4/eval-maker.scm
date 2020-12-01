;
; Creates at the runtime immediately invoked function
; including there the given list of include strings
; and evaluating body procedure defined in one of
; those includes or globally with the given
; symbolic name and no arguments.
;
; This maker allow you to create a huge construct
; without polluting the global scope with alot
; of stuff defined in the includes.
;
(define (eval-maker includes body-proc-symbol)
 (eval
  (list
   (append
    (list ;<— immediately invoked function
     'lambda
     '()
    )
    (map (lambda (in) (list 'include in)) includes)
    (list (list body-proc-symbol))
   )
  )
 )
)
