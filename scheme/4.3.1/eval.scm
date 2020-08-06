;
; See comments in «4.1.1/eval.scm» — they are removed from here.
;
(define-macro (make-eval global-env includes eval-body-expr)
 `(make-eval-impl ,global-env ,includes (quote ,eval-body-expr))
)

(define-value-if-not 'underlying-eval eval)
(define-value-if-not 'underlying-apply apply)

(define (make-eval-impl global-env includes eval-body-expr-quoted)
 (define result-eval void)

 ; This script is the result of the evaluator being evaluated onwards.
 ; Arguments «eval-body-expr-quoted» ordinary calls for the function
 ; that creates nested evaluation environment and actually evalues
 ; the script in it. On the callbak see «eval-amb-routine.scm».
 (define eval-call-lambda
  (list
   'lambda
   ; Note the new argument «callback» — see «eval-amb-routine.scm»:
   '(callback exp . env-arg)
   '(define env (if (null? env-arg) global-env (car env-arg)))
   eval-body-expr-quoted
  )
 )

 (set! make-env-temp global-env)

 (set! result-eval
  (underlying-eval ;<— evaluate the evaluator
   (list  ;<— immediately invoked function
    (append
     (list 'lambda '() '(define global-env make-env-temp))
     (map (lambda (file) (list 'include file)) includes)
     (list eval-call-lambda)
    )
   )
  )
 )

 (set! make-env-temp void)

 result-eval
)

; Special variable that holds «global-env» argument
; during the evaluation of the evaluator:
(define make-env-temp void)
