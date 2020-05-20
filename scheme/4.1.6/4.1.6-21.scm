(include "../3.3.2/assert.scm")

(define (log . args) (for-each display args) (newline))

(assert-eq? 3628800
 (
  (lambda (n)
   (
    (lambda (fact)
     (fact fact n)
    )

    (lambda (fact n)
     (if (= 1 n) 1
      (* n (fact fact (- n 1)))
     )
    )
   )
  )
  10
 )
)

; Y-operator for recursive function with one argument.
(define (Y1 λ)
 (lambda (n)
  (
   (lambda (λ) (λ λ n))
   λ
  )
 )
)

(assert-eq? 3628800
 (
  (Y1
   (lambda (fact n)
    (if (= 1 n) 1
     (* n (fact fact (- n 1)))
    )
   )
  )
  10
 )
)

; Recursive version of Fibonacci.
(assert-eq? 55
 (
  (Y1
   (lambda (fib n)
    (cond
     ((= n 0) 0)
     ((= n 1) 1)
     (else
      (+
       (fib fib (- n 2))
       (fib fib (- n 1))
      )
     )
    )
   )
  )
  10
 )
)

(define (fib i a b)
 (if (= i 0) a
  (fib (- i 1) b (+ a b))
 )
)

; Y1 operator with two initial parameters.
(define (Y1-ab a b λ)
 (lambda (n)
  (
   (lambda (λ) (λ λ n a b))
   λ
  )
 )
)

; Corecursive (iterative) version of Fibonacci.
(assert-equal? 354224848179261915075
 (
  (Y1-ab 0 1
   (lambda (fib i a b)
    (if (= i 0) a
     (fib fib (- i 1) b (+ a b))
    )
   )
  )
  100
 )
)

; Task «b» of 4.21.
(define (f x) ;<— it's actually even?
 (
  (lambda (even? odd?)
   (even? even? odd? x)
  )

  (lambda (even? odd? n)
   (if (= 0 n) #t (odd? even? odd? (- n 1)))
  )

  (lambda (even? odd? n)
   (if (= 0 n) #f (even? even? odd? (- n 1)))
  )
 )
)

(assert-true?  (f 10))
(assert-false? (f 11))
(assert-true?  (f 12))
(assert-false? (f 1))
(assert-true?  (f 2))
