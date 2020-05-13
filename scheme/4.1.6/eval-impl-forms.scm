
; Direct implementation of «let» form, transformation
; to lambda looks messy in this case, compare it with
; eval-inner variant in «4.1.2-6.scm».
(define eval-disp-form-let
 (
  (lambda () ;<— immediately invoked function
   ;
   ; If you compare this implementation with «4.1.2-6.scm»,
   ; you see no dynamic evaluation here. This is because
   ; here we are in the underlying evaluator, thus we have
   ; only the given environment, but no own one as inside
   ; the procedure of the registed form.
   ;
   (define (let-form exp env)
    ; The list of variable names created for «let»:
    (define vars (map car (cadr exp)))

    ; And independently evaluate the values:
    (define vals
     (map 
      (lambda (exp) (eval-impl (cadr exp) env))
      (cadr exp)
     )
    )

    ; Then we extend environment:
    (define env-ext (extend-environment vars vals env #f))

    ; Provide some info on the new environment:
    (if-debug (env-info-add env-ext 'let vars))

    ; Now we evaluate the body in the extended env:
    (eval-impl (eval-wrap-begin (cddr exp)) env-ext)
   )

   (eval-disp-register-form 'let let-form)
   let-form ;<— resulting form
  )
 )
)

(define eval-disp-form-and
 (
  (lambda () ;<— immediately invoked function
   (define (next exp env)
    (if (null? exp) #t
     (if (eval-impl (car exp) env)
      (next (cdr exp) env)
      #f
     )
    )
   )

   (define (and-form exp env)
    (next (cdr exp) env)
   )

   (eval-disp-register-form 'and and-form)
   and-form ;<— resulting form
  )
 )
)

(define eval-disp-form-or
 (
  (lambda () ;<— immediately invoked function
   (define (next exp env)
    (if (null? exp) #f
     (if (eval-impl (car exp) env)
      #t
      (next (cdr exp) env)
     )
    )
   )

   (define (or-form exp env)
    (next (cdr exp) env)
   )

   (eval-disp-register-form 'or or-form)
   or-form ;<— resulting form
  )
 )
)

(define (make-promise exp env)
 (list 'promise exp env)
)

(define (eval-promise? x)
 (and
  (list? x)
  (= 3 (length x))
  (eq? 'promise (car x))
 )
)

; Implements delay-form used for infinite streams.
(define eval-disp-form-delay
 (
  (lambda () ;<— immediately invoked function
   (define (eval-delay exp env)
    (make-promise (cadr exp) env)
   )

   (eval-disp-register-form 'delay eval-delay)
   eval-delay ;<— resulting form
  )
 )
)

(define eval-disp-eval-promise
 (
  (lambda () ;<— immediately invoked function
   (define (eval-promise exp env)
    (define p (eval-impl (cadr exp) env))
    (eval-impl (cadr p) (caddr p))
   )

   (eval-disp-register-form 'eval-promise eval-promise)
   eval-promise ;<— resulting form
  )
 )
)

(define (resolved-promise? x)
 (and
  (list? x)
  (= 3 (length x))
  (eq? 'promise (car x))
  (eq? 'resolved (cadr x))
 )
)

(define (get-resolved-value promise)
 (caddr promise)
)

(define (resolve-promise p)
 (define result (eval-impl (cadr p) (caddr p)))

 ; Memoize the value in the same record:
 (set-car! (cdr p) 'resolved)
 (set-car! (cddr p) result)

 result
)

(define (eval-force p)
 (cond
  ((resolved-promise? p)
   (get-resolved-value p)
  )

  ((eval-promise? p)
   (resolve-promise p)
  )

  (else (error "Not a promise to force" p))
 )
)

; Instead of using Gambit Scheme macros, as we use to in
; «3.5.1/stream.scm», here we create dedicated form for this.
(define eval-disp-cons-stream
 (
  (lambda () ;<— immediately invoked function
   (define (cons-stream exp env)
    (cons
     (eval-impl (cadr exp) env)
     (make-promise (caddr exp) env)
    )
   )

   (eval-disp-register-form 'cons-stream cons-stream)
   cons-stream ;<— resulting form
  )
 )
)
