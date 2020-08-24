;
; This file contains test cases common for all evaluators.
;
(eval-basic (debug on))

; Amb evaluator: self-evaluated values.
(assert-eq? 123 (eval-basic 123))
(assert-equal? 123.0 (eval-basic 123.0))
(assert-eq? #t (eval-basic #t))
(assert-eq? #f (eval-basic #f))
(assert-eq? #\A (eval-basic #\A))

; Amb evaluator: access global variable.
(amb-eval-define 'abc 'ABC)
(assert-eq? 'ABC (eval-basic abc))

; Amb evaluator: stop on single value.
(assert-equal? (list 123 void) (eval-amb-list 123))

; Amb evaluator: call primitive functions.
(assert-eq? 3 (eval-basic (+ 1 2)))
(assert-equal? '(1 . 2) (eval-basic (cons 1 2)))
(assert-equal? '(1 2 3) (eval-basic (list 1 2 3)))

; Amb evaluator: quotation.
(assert-eq? 'abc (eval-basic (quote abc)))
(assert-eq? 'abc (eval-basic 'abc))
(assert-equal? '(a b c) (eval-basic '(a b c)))

; Amb evaluator: if.
(assert-eq? 1 (eval-basic (if (> 2 1) 1 2)))
(assert-eq? 2 (eval-basic (if (< 2 1) 1 2)))

; Amb evaluator: conditions.
(assert-eq? 1 (eval-basic (cond ((= 1 1) 1))))
(assert-eq? 2 (eval-basic (cond ((= 1 2) 1) ((= 2 2) 2))))
(assert-eq? 3 (eval-basic (cond ((= 1 2) 1) ((= 2 1) 2) (else 3))))

; Amb evaluator: simple definitions.
(assert-eq? 123
 (eval-basic
  (define a 100)
  (define b 20)
  (define s (+ a b))
  (+ s 3)
 )
)

; Amb evaluator: function definition.
(assert-eq? 123
 (eval-basic
  (define (plus a b) (+ a b))
  (plus 100 23)
 )
)

; Amb evaluator: recursive function.
(assert-eq? 24
 (eval-basic
  (define (factorial n)
   (if (= n 1) (debug log-env "\n—— Factorial resursion ——"))
   (if (= n 1) 1 (* n (factorial (- n 1))))
  )
  (factorial 4)
 )
)

; Amb evaluator: recursive function with two calls.
(assert-equal? 55
 (eval-basic
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
 (eval-basic
  (define sum (lambda (a b) (+ a b)))
  (sum 2 3)
 )
)

; Amb evaluator: lambda clojure.
(assert-eq? 5
 (eval-basic
  (define (make-sum a)
   (lambda (b) (+ a b))
  )

  (define sum (make-sum 2))
  (sum 3)
 )
)

; Amb evaluator: assignment.
(assert-eq? 10
 (eval-basic
  (define (make-acc n)
   (lambda (v) (set! n (+ n v)))
  )

  (define acc (make-acc 0))
  (acc 2) (acc 3) (acc 5) ;<— set! returns the value
 )
)

; Amb evaluator: begin.
(assert-eq? 10
 (eval-basic
  (define sum 0)
  (begin
   (set! sum 4)
   (+ sum 6)
  )
 )
)

; Basic evaluator: global definition with clojure.
(eval-basic
 (define my-value 'Abc)
 (define (get-value) my-value)
 (global Global get-value)
)

(assert-equal? 'Abc (eval-basic (Global)))

; Amb evaluator: apply form.
(assert-eq? 123
 (eval-basic
  (apply + '(100 20 3))
 )
)

(assert-eq? 123
 (eval-basic
  (apply + (list 100 20 3))
 )
)

(assert-eq? 123
 (eval-basic
  (apply + (append '(100) '(20) '(3)))
 )
)

(assert-eq? 123
 (eval-basic
  (define (sum a b c) (+ a b c))
  (apply sum '(100 20 3))
 )
)

; Amb evaluator: variable length arguments
(assert-eq? 123
 (eval-basic
  (define (sum a . bc)
   (apply + (cons a bc))
  )

  (sum 100 20 3)
 )
)

(assert-eq? 123
 (eval-basic
  (define (sum a b . c)
   (apply + (cons a (cons b c)))
  )

  (sum 100 20 3)
 )
)

(assert-eq? 123
 (eval-basic
  (define (sum . args)
   (apply + args)
  )

  (sum 100 20 3)
 )
)

; Amb evaluator: variable length arguments for lambdas
(assert-eq? 123
 (eval-basic
  (define sum
   (lambda (a . bc) (apply + (cons a bc)))
  )

  (sum 100 20 3)
 )
)

(assert-eq? 123
 (eval-basic
  (define sum
   (lambda (a b . c) (apply + (cons a (cons b c))))
  )

  (sum 100 20 3)
 )
)

(assert-eq? 123
 (eval-basic
  (define sum
   ; BIG Note: this variant does not work in Gambit Scheme!
   ; But for our evaluator we have supported it...
   (lambda (. args) (apply + args))
  )

  (sum 100 20 3)
 )
)

; Amb evaluator: and form.
(assert-true?
 (eval-basic
  (and (eq? 'abc 'abc) (= 1 1))
 )
)

(assert-false?
 (eval-basic
  (and (eq? 'abc 'abc) (= 1 2))
 )
)

(assert-false?
 (eval-basic
  (and (= 1 2) (error "Wrong!"))
 )
)

; Amb evaluator: or form.
(assert-true?
 (eval-basic
  (or (eq? 'abc 'def) (= 1 1))
 )
)

(assert-false?
 (eval-basic
  (or (eq? 'abc 'def) (= 1 2))
 )
)

(assert-true?
 (eval-basic
  (or (= 1 1) (error "Wrong!"))
 )
)

; Amb evaluator: «let» form.
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
     (debug log-env "\n" "—— Environments of three nested lets: ")
     (+ a b c)
    )
   )
  )
 )
)

; Standard library: mapping with primitive function.
(assert-equal? '(1 4 9)
 (eval-basic (map square '(1 2 3)))
)

(assert-equal? '(a b c)
 (eval-basic
  (map car '((a 1) (b 2) (c 3)))
 )
)

(assert-equal? '((a 1) (b 2) (c 3))
 (eval-basic
  (map list '(a b c) '(1 2 3))
 )
)

; Standard library: mapping with defined function.
(assert-equal? '(111 222 333)
 (eval-basic
  (define (sum . args) (apply + args))
  (map sum '(100 200 300) '(10 20 30) '(1 2 3))
 )
)

; Standard library: mapping with lambda.
(assert-equal? '(111 222 333)
 (eval-basic
  (map
   (lambda (. args) (apply + args))
   '(100 200 300) '(10 20 30) '(1 2 3)
  )
 )
)

; Standard library: for each.
(assert-equal? '(5 4 3 2 1)
 (eval-basic
  (define (rev seq)
   (define s '())
   (for-each
    (lambda (item)
     (set! s (cons item s))
    )
    seq
   )
   s ;<— resulting value
  )

  (rev '(1 2 3 4 5))
 )
)

(log "\n" "Amb Evaluator [Core] of §4.3.1 successfully tested!" "\n")
