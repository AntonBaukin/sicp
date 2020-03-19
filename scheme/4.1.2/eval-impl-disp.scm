;
; Evaluator from task 4.3 that uses table of special forms.
; For this table we reuse class ops EvalEnvFrame from
; the environment — variable «eval-disp-table».
(define eval-disp-table ((table-op-make EvalEnvFrame)))

(define (eval-disp exp env)
 (cond
  ; Integral primitives may not be dispatched:
  ((self-evaluating? exp)
   exp
  )

  ((variable? exp)
   (lookup-variable exp env)
  )

  ; Delegation to the dispatcher:
  ((eval-disp? exp)
   (eval-dispatch exp env)
  )

  (else (eval-disp-else exp env))
 )
)

(define (eval-disp? exp)
 (and (list? exp) (symbol? (car exp)))
)

(define (eval-dispatch exp env)
 (define form (env-frame-table-lookup eval-disp-table (car exp)))

 (if (not (eq? void form))
  (form exp env)
  ; We found no special form — treat as function call:
  (eval-disp-else exp env)
 )
)

; As in basic evaluator, apply calls are any lists,
; and thay may not be dispatched either.
(define (eval-disp-else exp env)
 (apply-impl
  (eval-impl (operator exp) env)
  (list-of-values (operands exp) env)
  env
  exp
 )
)

; Register takes arguments (form-symbol form-proc ...)
; and adds the form processors to the table.
(define (eval-disp-register . forms)
 (define (next tail)
  (env-frame-table-add
   eval-disp-table
   (cadr tail) ;<— first comes the table value
   (car tail)  ;<- then the key, form symbol
  )

  (if (not (null? (cddr tail)))
   (next (cddr tail))
  )
 )

 (next forms)
)
