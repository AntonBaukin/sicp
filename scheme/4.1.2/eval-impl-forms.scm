(define eval-disp-basic-forms
 (list
  'quote ;<— yes, Apple style...

  (lambda (exp env)
   (text-of-quotation exp)
  )

   'define

  (lambda (exp env)
   (eval-definition exp env)
  )

  'if

  (lambda (exp env)
   (eval-if exp env)
  )

  'cond

  (lambda (exp env)
   (eval-impl (cond->if exp) env)
  )

  'set!

  (lambda (exp env)
   (eval-assignment exp env)
  )

  'begin

  (lambda (exp env)
   (eval-sequence (begin-actions exp) env)
  )

  'global

  (lambda (exp env)
   (eval-global-definition exp env)
  )

  'lambda

  (lambda (exp env)
   (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env
   )
  )

  'eval

  (lambda (exp env)
   ; Here we invoke the evaluator twice. Inner call resolves
   ; the expression that gives us the expression to evaluate
   ; at the second call.
   (eval-impl (eval-impl (cadr exp) env) env)
  )

  ; Regular evaluator is invoked in the context of the procedure
  ; that calls eval-form. This may be not the context we want.
  ; In cases, such as defining new forms with register, we
  ; have to address the environment stored somewhere in the
  ; call-one (up the environments chain, zero frames).
  ;
  ; Evaluation with dynamic context switch takes expression
  ; list of two items: (env-select-exp body-exp), where
  ; first expression selects the environmane — mostly,
  ; this is the name of the variable, such as 'env.
  ;
  ; Samples are in the tasks of let-forms 4.1.2-6 and 4.1.2-7.
  ;
  'eval-dynamic

  (lambda (exp env)
   (define ext-env (eval-impl (cadr exp) env))
   (define eval-env (merge-envs ext-env env))
   (eval-impl (caddr exp) eval-env)
  )

  ; Helper function that evaluates the given expression while
  ; it returns not void, on else value breaks the loop and
  ; returns previous value as the overall result.
  ;
  ; Note: this form is usefull as our evaluator has no
  ; tail recursion optimization.
  ;
  'eval-loop

  (lambda (exp env)
   (define body
    (if (= 1 (length (cdr exp)))
     (cadr exp)
     (append (list 'begin) (cdr exp))
    )
   )

   (eval-loop body env)
  )

  'apply apply-call

  'register eval-disp-register-gateway

  'debug

  (lambda (exp env)
   (debug-call env exp)
  )
 )
)
