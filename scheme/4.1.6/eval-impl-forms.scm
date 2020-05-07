
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
