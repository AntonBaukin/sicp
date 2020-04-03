(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

; For «register» see «4.1.2-4.a.scm».
(eval-basic (register
 let ;<— we do not to quote it

 ; On this implementation and dynamic evaluation,
 ; see comments for task 4.1.2-6.
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

  (define (named-let? exp)
   (symbol? (cadr exp))
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

  (define (named-let->lambda name clauses body)
   (append
    (list
    (list
     (list
      'lambda
      '()
      (append
       (list 'define (cons name (map car clauses)))
       (list (make-eval-dynamic body))
      )
      name
     )
    )
    )
    (map make-eval-dynamic (map cdr clauses))
   )
  )

  (eval
   (if (named-let? exp)
    (named-let->lambda (cadr exp) (caddr exp) (cdddr exp))
    (let->lambda (cadr exp) (cddr exp))
   )
  )
 )
))

; The following tests are from ordinary let, task 4.1.2-6:

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

; Named let tests:

(assert-eq? 1
 (eval-basic
  (let abc ((a 1) (b 2))
   (if (> a 0)
    (abc (- a 1) (- b 1))
    (+ a b)
   )
  )
 )
)

(assert-equal? 55
 (eval-basic
  (define (fib n)
   ; Here «n» is in the scope of «fib», thus we have to
   ; use dynamic evaluation to be able to get it let:
   (let fib-iter ((a 1) (b 0) (count n))
    (if (= 0 count) b
     (fib-iter (+ a b) a (- count 1))
    )
   )
  )

  (fib 10)
 )
)
