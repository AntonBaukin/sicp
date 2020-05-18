(include "filter.scm")

(define eval-disp-form-letrec
 (
  (lambda () ;<— immediately invoked function
   (define (letrec->let-params exp)
    (map (lambda (def) (list (car def) ''*unassigned*)) (cadr exp))
   )

   (define (letrec->let-body exp)
    (append
     (map
      (lambda (def) (list 'set! (car def) (cadr def)))
      (cadr exp)
     )
     (cddr exp)
    )
   )

   (define (letrec-form exp env)
    (eval-impl
     (append (list 'let (letrec->let-params exp))
      (letrec->let-body exp)
     )
     env
    )
   )

   (eval-disp-register-form 'letrec letrec-form)
   letrec-form ;<— resulting form
  )
 )
)
