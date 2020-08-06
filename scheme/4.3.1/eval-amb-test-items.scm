(eval-amb-result (debug on))

; Amb evaluator: self-evaluated values.
(assert-eq? 123 (eval-amb-result 123))
(assert-equal? 123.0 (eval-amb-result 123.0))
(assert-eq? #t (eval-amb-result #t))
(assert-eq? #f (eval-amb-result #f))
(assert-eq? #\A (eval-amb-result #\A))

; Amb evaluator: access global variable.
(amb-eval-define 'abc 'ABC)
(assert-eq? 'ABC (eval-amb-result abc))

; Amb evaluator: stop on single value.
(assert-equal? (list 123 void) (eval-amb-list 123))

; Amb evaluator: call primitive functions.
(assert-eq? 3 (eval-amb-result (+ 1 2)))
(assert-equal? '(1 . 2) (eval-amb-result (cons 1 2)))
(assert-equal? '(1 2 3) (eval-amb-result (list 1 2 3)))

; Amb evaluator: quotation.
(assert-eq? 'abc (eval-amb-result (quote abc)))
(assert-eq? 'abc (eval-amb-result 'abc))
(assert-equal? '(a b c) (eval-amb-result '(a b c)))

; Amb evaluator: if.
(assert-eq? 1 (eval-amb-result (if (> 2 1) 1 2)))
(assert-eq? 2 (eval-amb-result (if (< 2 1) 1 2)))

; Amb evaluator: conditions.
(assert-eq? 1 (eval-amb-result (cond ((= 1 1) 1))))
(assert-eq? 2 (eval-amb-result (cond ((= 1 2) 1) ((= 2 2) 2))))
(assert-eq? 3 (eval-amb-result (cond ((= 1 2) 1) ((= 2 1) 2) (else 3))))

; Amb evaluator: simple definitions.
(assert-eq? 123
 (eval-amb-result
  (define a 100)
  (define b 20)
  (define s (+ a b))
  (+ s 3)
 )
)

; Amb evaluator: function definition.
(assert-eq? 123
 (eval-amb-result
  (define (plus a b) (+ a b))
  (plus 100 23)
 )
)

; Amb evaluator: recursive function.
(assert-eq? 24
 (eval-amb-result
  (define (factorial n)
   (if (= n 1) (debug log-env "\n—— Factorial resursion ——"))
   (if (= n 1) 1 (* n (factorial (- n 1))))
  )
  (factorial 4)
 )
)

; Amb evaluator: recursive function with two calls.
(assert-equal? 55
 (eval-amb-result
  (define (fib n)
   (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 2)) (fib (- n 1))))
   )
  )

  (fib 10)
 )
)

; Amb evaluator: basic lambda.
(assert-eq? 5
 (eval-amb-result
  (define sum (lambda (a b) (+ a b)))
  (sum 2 3)
 )
)

; Amb evaluator: lambda clojure.
(assert-eq? 5
 (eval-amb-result
  (define (make-sum a)
   (lambda (b) (+ a b))
  )

  (define sum (make-sum 2))
  (sum 3)
 )
)
