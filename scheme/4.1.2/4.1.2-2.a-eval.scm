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

  ; So, Hugo wants a function call to be treated here...
  ((application? exp)
   (apply-impl
    (eval-impl (operator exp) env)
    (list-of-values (operands exp) env)
    env
    exp
   )
  )

  ((definition? exp)
   (eval-definition exp env)
  )

  ((if? exp)
   (eval-if exp env)
  )

  ((cond? exp)
   (eval-impl (cond->if exp) env)
  )

  ((assignment? exp)
   (eval-assignment exp env)
  )

  ((begin? exp)
   (eval-sequence (begin-actions exp) env)
  )

  ((lambda? exp)
   (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env
   )
  )

  ((debug-command? exp)
   (if debug-mode? (debug-call exp env))
  )
 )
)