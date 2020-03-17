
; Unline SICP, we do not treat specially primitive
; procedures. Instead, we install them in the global
; environment — thus we allow to redefine them as it's
; possible in Gambit Scheme.
(define (apply-basic procedure arguments env exp)
 (if (compound-procedure? procedure)
  (apply-basic-compound procedure arguments env exp)
  (underlying-apply procedure arguments)
 )
)

(define (apply-basic-compound procedure arguments env exp)
 (define nest? (same-procedure-env? env procedure))

 (define call-env
  (extend-environment
   (procedure-parameters procedure)
   arguments
   (if nest? env (procedure-environment procedure))
   nest?
  )
 )

 (if (not nest?)
  (begin
   ; Tag the environment with the procedure name:
   (if (symbol? (operator exp))
    (env-info-add call-env (operator exp))
   )

   ; Allow track recursive calls:
   (env-info-add call-env procedure)
  )
 )

 (eval-sequence (procedure-body procedure) call-env)
)

; If the given environment was created for the given procedure,
; we call it recursivele, and we do not need new environment,
; but must reuse the same one, creating the nested frame.
;
; Note: that environment [list] differs when nesting, but
; it's id and info stay the same.
;
(define (same-procedure-env? env procedure)
 (env-info-has? env procedure)
)

(define (eval-sequence exps env)
 (if (null? (cdr exps))
  (eval-impl (car exps) env)
  (begin
   (eval-impl (car exps) env)
   (eval-sequence (cdr exps) env)
  )
 )
)
