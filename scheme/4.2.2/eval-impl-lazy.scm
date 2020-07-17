;
; With lazy evaluation, we postpone computation of the operands.
; We do not call analyzed argument procedures, «aps», but pass
; them to thunk and resolve inside «apply-basic-lazy()».
;
(define eval-apply-call-aps-lazy
 (
  (lambda () ;<— immediately invoked function
   (define (lazy-aps aps env) aps)
   (set! eval-apply-call-aps lazy-aps)
   lazy-aps ;<— resulting function
  )
 )
)

; Instead of calling execution procedures,
; we postpone their computation with thunks.
(define (thunk-aps aps env)
 (thunk-them aps env)
)

; We resolve each thunk before invoking a primitive.
(define (resolve-aps aps env)
 (map
  ; Some may be: 1) analyzed procedure, 2) a thunk,
  ; or 3) a direct value.
  (lambda (some)
   (resolve-value
    (if (procedure? some) (some env) some)
   )
  )
  aps
 )
)

; Here we replace invocation routine of a basic procedure.
; On reset primitives of «prime-ops-cXr» — see «eval-lazy-cxr.scm».
(define apply-basic-lazy
 (
  (lambda () ;<— immediately invoked function
   (define (apply-basic-lazy procedure aps env exp)
    ; Resolve procedure thunk before testing it:
    (set! procedure (resolve-value procedure))

    (if (compound-procedure? procedure)
     (apply-basic-compound procedure (thunk-aps aps env) env exp)
     (underlying-apply procedure (resolve-aps aps env))
    )
   )

   (set! apply-basic apply-basic-lazy)
   (set! prime-ops-cXr '()) ;<— revoke prime c..r

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

(define debug-log-eval-msg-value-lazy
 (
  (lambda () ;<— immediately invoked function
   (define (eval-msg-value exp env)
    (resolve-value (eval-impl exp env))
   )

   (set! debug-log-eval-msg-value eval-msg-value)
   eval-msg-value ;<— resulting function
  )
 )
)
