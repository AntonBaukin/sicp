
(define eval-disp-apply-lazy
 (
  (lambda () ;<— immediately invoked function
   (define (apply-lazy exp)
    ; As we always require the operator, we analyze it ahead:
    (define fp (eval-analyze (operator exp)))

    ; Here we also analyze the operands. With lazy evaluation,
    ; this stage might be also postponed, but we take it:
    (define aps (map eval-analyze (operands exp)))

    (lambda (env)
     (apply-impl
      ; We resolve the thunk of function instance:
      (resolve-value (fp env))
      ; Instead of calling execution procedures,
      ; we postpone their computation with thunks:
      (thunk-them aps env)
      env
      exp
     )
    )
   )

   ; Replace apply from analyzing evaluator:
   (set! eval-disp-apply apply-lazy)
   apply-lazy ;<— resulting function
  )
 )
)

; Here we replace invocation routine of a basic procedure.
; On reset primitives of «prime-ops-cXr» — see «eval-lazy-cxr.scm».
(define apply-basic-lazy
 (
  (lambda () ;<— immediately invoked function
   (define (apply-basic-lazy procedure arguments env exp)
    ; Resolve procedure thunk before testing it:
    (set! procedure (resolve-value procedure))

    (if (compound-procedure? procedure)
     (apply-basic-compound procedure arguments env exp)
     ; We resolve each thunk before invoking a primitive:
     (underlying-apply procedure (map resolve-value arguments))
    )
   )

   (set! apply-basic apply-basic-lazy)
   (set! prime-ops-cXr '())

   apply-basic-lazy ;<— resulting function
  )
 )
)

; Support thunks when printing debug values.
(define debug-log-describe-var-value-lazy
 (
  (lambda () ;<— immediately invoked function
   (define var-value-impl debug-log-describe-var-value)

   (define (var-value value)
    (var-value-impl (resolve-value value))
   )

   (set! debug-log-describe-var-value var-value)
   var-value ;<— resulting function
  )
 )
)
