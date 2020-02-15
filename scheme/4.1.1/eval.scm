(include "../2.5.1/defined.scm")
(include "eval-env.scm")

; Works in Gambit Scheme. Macros makes make-eval-impl
; function to be like a special form.
(define-macro (make-eval global-env includes eval-body-expr)
 `(make-eval-impl ,global-env ,includes (quote ,eval-body-expr))
)

; Evaluator of underlying Scheme interpreter.
; Saved in the case you redefine it globally.
(define-value-if-not 'underlying-eval eval)
(define-value-if-not 'underlying-apply apply)

; This implements special Eval-Apply procedure as it's started
; in SICP §4.1.1. Make eval function takes delayed expression
; that is invoked in the context of local definitions of lang
; supporting routines.
;
; This implementation is extended in various tasks of SICP
; preserving the backward compatibility.
;
; The key trick and the task here is to inject eval body
; as an expression into the closure where all supporting
; definitions are directly available by their names. This
; allows to write evaluator without redefining the common
; code base and without injecting that names from a list
; of functions — interfaces commonly used in Chapter 3.
; (Consider «2.3.3/tree.scm» as a sample.)
;
; Argument «includes» is the list of files to include.
; Here we have to make walkaround as global evaluation
; looses the origin of calling file. So, we have to use
; root-relative paths, as: "../4.1.1/eval-impl.scm".
; It works in our samples, but is not a general way...
;
(define (make-eval-impl global-env includes eval-body-expr-quoted)
 (define result-eval void)

 (define eval-call-lambda
  (list
   'lambda
   '(exp . env-arg)
   '(define env (if (null? env-arg) global-env (car env-arg)))
   eval-body-expr-quoted
  )
 )

 ; Temporary assign to global target environment.
 (set! make-env-temp global-env)

 (set! result-eval
  (underlying-eval
   (list  ;<— immediately invoked function
    (append
     (list ;   to protect the global scope
      'lambda
      '()

      ; Nice trick! Just refer our temporary holder:
      '(define global-env make-env-temp)
     )

     ; This injects the evaluator implementation from
     ; the given files into external closure of the
     ; resulting evaluator.
     (map
      (lambda (file) (list 'include file))
      includes
     )

     ; And here comes the resulting evaluator:
     (list eval-call-lambda)
    )
   )
  )
 )

 ; And clean it...
 (set! make-env-temp void)

 result-eval
)

; Special variable that holds «global-env» argument
; of make-eval in the underlying global environment.
; In Gambit we can't evaluate in the environment of
; a procedure invoked, i.e. in the closure.
(define make-env-temp void)

