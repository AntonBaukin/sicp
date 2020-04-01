(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

; For «register» see «4.1.2-4.a.scm».
(eval-basic (register
 let ;<— we do not to quote it

 (lambda (exp env) ;<— let-lambda
  (define (wrap-begin exp)
   (if (= 1 (length exp))
    (car exp)
    (append '(begin) exp)
   )
  )

  ; In this case we create lambda in the parent context of
  ; let-lambda, but not in the call context of lambda we
  ; construct here. So, we don't see it's arguments...
  ;
  ; See the details of the dynamic evaluator defined
  ; in «eval-impl-forms.scm».
  ;
  ; We select environment by standard name.
  ; This is the environment of our lambda.
  (define (make-eval-dynamic exp)
   (list 'eval-dynamic 'env (wrap-begin exp))
  )

  (define (let->lambda clauses body)
   (append
    (list
     (cons
      'lambda
      (list
       (map car clauses)
       (make-eval-dynamic body)
      )
     )
    )

    ; We also dynamically evaluate the initialization expressions
    ; as they may refer the variables of outer let blocks:
    (map make-eval-dynamic (map cdr clauses))
   )
  )

  (eval (let->lambda (cadr exp) (cddr exp)))
 )
))

(assert-eq? 123
 (eval-basic
  (let (
    (a 100)
    (b 20)
    (c 3)
   )
   (+ a b c)
  )
 )
)

(assert-eq? -123
 (eval-basic
  (let (
    (a 100)
    (b 20)
    (c 3)
   )
   (set! a (- a))
   (set! b (- b))
   (set! c (- c))
   (+ a b c)
  )
 )
)

(assert-eq? 110
 (eval-basic
  ((lambda (a) (let ((b 10)) (+ a b))) 100)
 )
)

(assert-eq? 111
 (eval-basic
  (let ((a 100))
   (let ((b (/ a 10)))
    (+ a b 1)
   )
  )
 )
)

(assert-eq? 111
 (eval-basic
  (let ((a 100))
   (let ((b (/ a 10)))
    (let ((c (/ a (* b b))))
     (+ a b c)
    )
   )
  )
 )
)
