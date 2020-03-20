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

  'register eval-disp-register-gateway

  'debug

  (lambda (exp env)
   (if debug-mode? (debug-call env exp))
  )
 )
)
