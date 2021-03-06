(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

; Here we implement «let*» form directly via «let».
; For «register» see «4.1.2-4.a.scm».
(eval-basic (register
 let ;<— copy from task 4.1.2-6

 (lambda (exp env) ;<— let-lambda
  (define (wrap-begin exp)
   (if (= 1 (length exp))
    (car exp)
    (append '(begin) exp)
   )
  )
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
    (map make-eval-dynamic (map cdr clauses))
   )
  )

  (eval (let->lambda (cadr exp) (cddr exp)))
 )

 let* ;<— we do not to quote it

 (lambda (exp env)
  (define (let*->let clauses body)
   (append
    (list
     'let
     (list (list (caar clauses) (cadar clauses)))
    )
    (if (null? (cdr clauses))
     body
     (list (let*->let (cdr clauses) body))
    )
   )
  )

  ; See sample below that defines the need of wrapping
  ; dynamic evaluation.
  (eval-dynamic env (eval (let*->let (cadr exp) (cddr exp))))
 )
))

; The following tests are from ordinary let, task 4.1.2-6:

(assert-eq? 123
 (eval-basic
  (let* (
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
  (let* (
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

; This sample defines the need for outer eval-dynamic in let* form:
; it must be invoked in context of lambda (a) to access it's argument.
(assert-eq? 110
 (eval-basic
  ((lambda (a) (let* ((b 10)) (+ a b))) 100)
 )
)

(assert-eq? 111
 (eval-basic
  (let* ((a 100))
   (let* ((b (/ a 10)))
    (+ a b 1)
   )
  )
 )
)

(assert-eq? 111
 (eval-basic
  (let* ((a 100))
   (let* ((b (/ a 10)))
    (let* ((c (/ a (* b b))))
     (+ a b c)
    )
   )
  )
 )
)


; The following tests are specific for let* form:

(assert-eq? 123
 (eval-basic
  (let* (
    (a 100)
    (b (/ a 5))
    (c (/ (+ a b) 40))
   )
   (+ a b c)
  )
 )
)

(assert-eq? 0
 (eval-basic
  (let* (
    (a 100)
    (b (/ a 5))
   )
   (let* (
     (c (/ (+ a b) 40))
     (d (- (+ a b c)))
    )
    (+ a b c d)
   )
  )
 )
)
