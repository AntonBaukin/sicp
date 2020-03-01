
(define (environment? env)
 (tagged-list? env 'environment)
)

(define (check-env env)
 (if (environment? env) env
  (error "Not an environment" env)
 )
)

(define (eval-extend-env env)
 (define frame ((table-op-make EvalEnvFrame)))

 ; Resulting environment object:
 (list 'environment (list frame) env)
)

(define (enclosing-environment env)
 (list-ref env 2)
)

(define env-frame-table-lookup
 (table-op-lookup EvalEnvFrame)
)

(define (first-frame env)
 (car (list-ref (check-env env) 1))
)

(define (env-frame-lookup var-name-symbol env)
 (env-frame-table-lookup
  (first-frame env)
  var-name-symbol
 )
)

(define (lookup-variable var-name-symbol env)
 (if (null? env)
  (error "Unbound variable name" var-name-symbol)
  (let ((v (env-frame-lookup var-name-symbol env)))
   ;(log "LOOK " var-name-symbol " @ " (first-frame env))
   (if (not (eq? void v)) v
    (lookup-variable
     var-name-symbol
     (enclosing-environment env)
    )
   )
  )
 )
)

(define env-frame-table-add
 (table-op-add EvalEnvFrame)
)

; Like add, but first checks existing item not to overwrite it.
; Returns #f on existed, void on success.
(define (env-frame-table-add-new table value key)
 (define v (env-frame-table-lookup table key))

 (if (not (eq? void v)) #f
  (begin
   (env-frame-table-add table value key)
   void
  )
 )
)

(define (define-variable var-name-symbol value env)
 ;(log "DEF " var-name-symbol " = " value)
 (env-frame-table-add
  (first-frame env)
  value
  var-name-symbol
 )
 ;(log "FRAME " (first-frame env))
)

; Special for defining primitives. Here definitions
; is a flat list of name value items.
;
; Warning: only new names are added,
; existing are not overwritten!
;
(define (define-variables env . definitions)
 (define frame (first-frame env))

 (define (next tail)
  (if (not (null? tail))
   (begin
    (env-frame-table-add-new frame (cadr tail) (car tail))
    (next (cddr tail))
   )
  )
 )

 (next definitions)
)

(define (eval-definition exp env)
 (define value (eval-impl (define-get-value exp) env))
 (define-variable (define-get-variable exp) value env)
 value ;<— and return this value, not 'ok as in SICP
)

(define (eval-in-nested-env evaluator exp env)
 (evaluator exp (eval-extend-env env))
)
