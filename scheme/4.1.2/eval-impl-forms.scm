;
; Definitions of the dispatcher forms.
;
(define (eval-disp-form-quote exp env)
 (text-of-quotation exp)
)

(define eval-disp-form-define
 eval-definition
)

(define eval-disp-form-if
 eval-if
)

(define (eval-disp-form-cond exp env)
 (eval-impl (cond->if exp) env)
)

(define eval-disp-form-set
 eval-assignment
)

(define (eval-disp-form-begin exp env)
 (eval-sequence (begin-actions exp) env)
)

(define eval-disp-form-global
 eval-global-definition
)

(define (eval-disp-form-lambda exp env)
 (make-procedure
  (lambda-parameters exp)
  (lambda-body exp)
  env
 )
)

; Here we invoke the evaluator twice. Inner call resolves
; the expression that gives us the expression to evaluate
; at the second call.
(define (eval-disp-form-eval exp env)
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
; first expression selects the environment — mostly,
; this is the name of the variable, such as 'env.
;
; Samples are in the tasks of let-forms 4.1.2-{6 7 8 9}.
;
(define (eval-disp-form-eval-dynamic exp env)
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
(define (eval-disp-form-loop exp env)
 (define body
  (if (= 1 (length (cdr exp)))
   (cadr exp)
   (append (list 'begin) (cdr exp))
  )
 )

 (eval-loop body env)
)

(define eval-disp-form-apply
 apply-call
)

(define eval-disp-form-register
 eval-disp-register-gateway
)

(define eval-disp-form-debug
 debug-call
)

(define eval-disp-basic-forms
 (list
  'quote ;<— yes, Apple style...
  eval-disp-form-quote

  'define
  eval-disp-form-define

  'if
  eval-disp-form-if

  'cond
  eval-disp-form-cond

  'set!
  eval-disp-form-set

  'begin
  eval-disp-form-begin

  'global
  eval-disp-form-global

  'lambda
  eval-disp-form-lambda

  'eval
  eval-disp-form-eval

  'eval-dynamic
  eval-disp-form-eval-dynamic

  'eval-loop
  eval-disp-form-loop

  'apply
  eval-disp-form-apply

  'register
  eval-disp-form-register

  'debug
  eval-disp-form-debug
 )
)
