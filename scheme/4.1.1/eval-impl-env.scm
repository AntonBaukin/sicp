
(define (environment? env)
 (tagged-list? env 'environment)
)

(define (check-env env)
 (if (environment? env) env
  (error "Not an environment" env)
 )
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

; Adds given to the info list of the environment.
; (Alters the info list instance.)
(define (env-info-add env . items)
 (eval-env-info-set
  env
  (reverse
   (append
    (reverse items)
    (reverse (eval-env-info env))
   )
  )
 )
)

(define (env-info-has? env item)
 (define (next info)
  (cond
   ((null? info) #f)
   ((eq? item (car info)) #t)
   (else (next (cdr info)))
  )
 )

 (next (eval-env-info env))
)

(define (env-info-miss? env item)
 (not (env-info-has? env item))
)

; Invokes iterator for each frame of the given environment.
; It takes: (frame index size); where «size» is the number
; of frames in the environment, «index» of top one is 0.
(define (for-each-frame env iter)
 (define frames (list-ref (check-env env) 1))
 (define size (length frames))

 (define (next frames index)
  (if (not (null? frames))
   (begin
    (iter (car frames) index size)
    (next (cdr frames) (+ 1 index))
   )
  )
 )

 (next frames 0)
)

(define (env-frame-lookup var-name-symbol env)
 (env-frame-table-lookup
  (first-frame env)
  var-name-symbol
 )
)

(define (lookup-special-global var-name-symbol env)
 (cond
  ((eq? 'void var-name-symbol) void)
  (else
   (if-debug
    (debug-log-env #f env "\n*** UNBOUND VARIABLE "
     (debug-escape var-name-symbol)
    )
   )
   (error "Unbound variable name" var-name-symbol)
  )
 )
)

(define (lookup-variable var-name-symbol ext-env)
 (define (lookup env)
  (if (null? env)
   (lookup-special-global var-name-symbol ext-env)
   (let ((v (env-frame-lookup var-name-symbol env)))
    (if (eq? void v) (lookup (enclosing-environment env)) v)
   )
  )
 )

 (lookup ext-env)
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

 value ;<— return the value
)

; Special for defining primitives. Here definitions
; is a flat list of name value items.
;
; Warning: only new names are added,
; existing are not overwritten!
;
(define (define-primitives env . definitions)
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

; Works like lookup, but replaces the variable, and returns
; the given value. Demands the variable to be defined.
(define (assign-variable env var-name-symbol value)
 (if (null? env)
  (error "Unbound variable name" var-name-symbol)
  (let ((v (env-frame-lookup var-name-symbol env)))
   (if (not (eq? void v))
    (begin
     (env-frame-table-add (first-frame env) value var-name-symbol)
     value ;<— return the new value
    )

    (assign-variable (enclosing-environment env) var-name-symbol value)
   )
  )
 )
)

; Adds or sets (replaces) variables of the top frame
; of the environment. Variables are a list of
; (name . value) pairs.
(define (define-variables env pairs)
 (define frame (first-frame env))

 (define (next tail)
  (if (not (null? tail))
   (begin
    (env-frame-table-add frame (cdar tail) (caar tail))
    (next (cdr tail))
   )
  )
 )

 (next pairs)
)

(define (eval-definition exp env)
 (define value (eval-impl (define-get-value exp) env))
 (define-variable (define-get-variable exp) value env)
 value ;<— and return this value, not 'ok as in SICP
)

; Here we evaluate the script line-by-line that allows
; us to mix top-level expressions in any order, just as
; any script file is executed.
(define (eval-in-nested-env evaluator script env)
 (define nested (eval-extend-env env))
 (define result void)

 (if-debug
  (env-info-add nested 'eval-private-scope)
  (if (env-info-miss? env 'global)
   (env-info-add env 'global)
  )
 )

 (for-each
  (lambda (exp)
   (set! result (evaluator exp nested))
  )
  script
 )

 result ;<— last evaluated expression
)

(define (extend-environment vars vals base-env nest?)
 (cond
  ((= (length vars) (length vals))
   (let ((env (extend-environment-impl base-env nest?)))
    (define-variables env (map cons vars vals))
    env ;<— resulting environment object
   )
  )

  ((< (length vars) (length vals))
   (error "Too many arguments supplied" vars vals)
  )

  (else (error "Too few arguments supplied" vars vals))
 )
)

(define (extend-environment-impl env nest?)
 ((if nest? eval-nest-env eval-extend-env) env)
)

(define (merge-envs parent child)
 (define (env->list env res)
  (if (null? env) res
   (env->list
    (enclosing-environment env)
    (cons env res)
   )
  )
 )

 (define (copy-env level env parent)
  (define copy (list-copy env))
  (define info (list-copy (list-ref env 3)))

  (set-car! (cddr copy) parent)
  (set-car! info level)
  (set-car! (cdddr copy) info)
  copy
 )

 (define (list->env level envs)
  (if (null? envs) '()
   (copy-env
    level
    (car envs)
    (list->env (- level 1) (cdr envs))
   )
  )
 )

 (define (merge pes ces res)
  (cond
   ((and (null? pes) (null? ces))
    res
   )

   ((null? pes)
    (merge '() (cdr ces) (cons (car ces) res))
   )

   ((and (not (null? ces)) (eval-env-eq? (car pes) (car ces)))
    (merge (cdr pes) (cdr ces) (cons (car pes) res))
   )

   (else
    (merge (cdr pes) ces (cons (car pes) res))
   )
  )
 )

 ; Hint: reversed, global comes first
 (define pes (env->list parent '()))
 (define ces (env->list child '()))

 ; Hint: reversed again, global comes last
 (define mes (merge pes ces '()))

 (list->env (- (length mes) 1) mes)
)
