
; Here we rewrite «analyze-sequence» as Pablo wants:
(define analyze-sequence-pablo
 (
  (lambda () ;<— immediately invoked function
   (define (seq a b)
    (lambda (env) (a env) (b env))
   )

   ; Pablo resolves each expression of sequence
   ; except the last one — the result.
   (define (resolve-analyze exp)
    (define proc (eval-analyze exp))
    (lambda (env)
     (resolve-value (proc env))
    )
   )

   (define (join exps)
    (if (null? (cdr exps))
     ; The last expression is not resolved:
     (eval-analyze (car exps))
     (seq
      (resolve-analyze (car exps))
      (join (cdr exps))
     )
    )
   )

   (set! analyze-sequence join)
   join ;<— resulting join
  )
 )
)
