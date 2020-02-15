;
; Here are difinitions for SICP Chapter 4 evaluator.
; They are included into closure environment of eval
; created in «make-eval-impl».
;
; It seems that this is the simplest way how to inject
; expression into existing closure in Gambit Scheme — as
; evaluation in the given environment is not supported.
;
; Warning! This implementation just resembles SICPS',
; but not exactly the same! The main difference is that
; it uses red-black tree tables for the frames, thus
; there is no mess with the lists here.
;
; More over. Not to overwrite everithing for single tasks,
; we created this evaluator to be modular of several files
; included — this file is the first of them.
;
(define (eval-basic exp env)
 (cond
  ((self-evaluating? exp)
   exp
  )

  ((variable? exp)
   (lookup-variable exp env)
  )

  ((quoted? exp)
   (text-of-quotation exp)
  )

  ((application? exp)
   (apply-impl
    (eval-impl (operator exp) env)
    (list-of-values (operands exp) env)
   )
  )
 )
)

; Evaluator reference to use in recursive calls.
(define eval-impl eval-basic)

; Unline SICP, we do not treat specially primitive
; procedures. Instead, we install them in the global
; environment — thus we allow to redefine them as it's
; possible in Gambit Scheme.
(define (apply-basic procedure arguments)
 (if (compound-procedure? procedure)
  (eval-sequence
   (procedure-body procedure)
   (extend-environment
    (procedure-parameters procedure)
    arguments
    (procedure-environment procedure)
   )
  )
  (underlying-apply procedure arguments)
 )
)

; Applicator reference to use in recursive calls.
(define apply-impl apply-basic)

(define (tagged-list? l tag)
 (and
  (list? l)
  (eq? tag (car l))
 )
)

; More than in SICP: number, boolean, string, character.
(define (self-evaluating? exp)
 (or
  (number? exp)
  (eq? #t exp)
  (eq? #f exp)
  (string? exp)
  (char? exp)
 )
)

(define (variable? exp)
 (symbol? exp)
)

(define (quoted? exp)
 (tagged-list? exp 'quote)
)

(define (text-of-quotation exp)
 (list-ref exp 1)
)

; See, how loose it is: it takes everything!
(define (application? exp)
 (list? exp)
)

(define (list-of-values exps env)
 (if (null? exps) '()
  (let ((value (eval-impl (car exps) env)))
   (cons value (list-of-values (cdr exps) env))
  )
 )
)

(define (operator exp)
 (car exp)
)

(define (operands exp)
 (cdr exp)
)

(define (compound-procedure? p)
 (tagged-list? p 'procedure)
)

(define (procedure-parameters p)
 (list-ref exp 1)
)

(define (procedure-body p)
 (list-ref exp 2)
)

(define (procedure-environment p)
 (list-ref exp 3)
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

(define (environment? env)
 (tagged-list? env 'environment)
)

(define (enclosing-environment env)
 (list-ref env 2)
)

(define env-frame-table-lookup
 (table-op-lookup EvalEnvFrame)
)

(define (check-env env)
 (if (environment? env) env
  (error "Not an environment" env)
 )
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
 (env-frame-table-add
  (first-frame env)
  value
  var-name-symbol
 )
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
