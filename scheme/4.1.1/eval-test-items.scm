(eval-basic (debug on))

; This form just returns the evaluated expression.
(define eval-exp (make-eval void '() exp))
(assert-eq? 123 (eval-exp 123))
(assert-eq? #t (eval-exp #t))
(assert-equal? '(1 2 3) (eval-exp '(1 2 3)))

; Second test that calls the environment.
(define eval-env-value 123)
(define (eval-env-get) eval-env-value)
(define eval-env (make-eval eval-env-get '() (env)))
(assert-eq? 123 (eval-env void))
(set! eval-env-value "abc")
(assert-equal? "abc" (eval-env void))

; Basic evaluator: self-evaluated values.
(assert-eq? 123 (eval-basic 123))
(assert-equal? 123.0 (eval-basic 123.0))
(assert-eq? #t (eval-basic #t))
(assert-eq? #f (eval-basic #f))
(assert-eq? #\A (eval-basic #\A))

; Basic evaluator: some primitive ops.
(assert-eq? 3 (eval-basic (+ 1 2)))
(assert-eq? 1 (eval-basic (- 2 1)))
(assert-eq? 6 (eval-basic (* 3 2)))
(assert-eq? 4 (eval-basic (/ 8 2)))
(assert-equal? '(1 . 2) (eval-basic (cons 1 2)))
(assert-equal? '(1 2 3) (eval-basic (list 1 2 3)))

; Basic evaluator: quotation.
(assert-eq? 'abc (eval-basic (quote abc)))
(assert-eq? 'abc (eval-basic 'abc))
(assert-equal? '(a b c) (eval-basic '(a b c)))

; Basic evaluator: access global variable.
(eval-env-define basic-evaluator-env 'abc 'ABC)
(assert-eq? 'ABC (eval-basic abc))

; Basic evaluator: simple definition.
(assert-eq? 'XYZ
 (eval-basic
  (define xyz 'XYZ)
  (debug log-env "—— Env after defining of xyz = 'XYZ ——")
  xyz
 )
)

; Basic evaluator: local scope of inner definitions.
(log "\n—— The following exception print is correct:")
(assert-error (lambda () (eval-basic xyz)) void)

; Basic evaluator: function definition.
(assert-eq? 123
 (eval-basic
  (define (plus a b) (+ a b))
  (plus 100 23)
 )
)

; Basic evaluator: recursive function.
(assert-eq? 24
 (eval-basic
  (define (factorial n)
   (if (= n 1) (debug log-env "\n—— Factorial resursion ——"))
   (if (= n 1) 1 (* n (factorial (- n 1))))
  )
  (factorial 4)
 )
)

; Basic evaluator: another recursive function.
(assert-equal? '(1 2 3 a b c)
 (eval-basic
  (define (append tail res)
   (if (null? tail) res
    (cons
     (car tail)
     (append (cdr tail) res)
    )
   )
  )

  (append '(1 2 3) '(a b c))
 )
)

; Basic evaluator: recursive function with two calls.
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

;
; Factorial print. As we see, we reuse the same procedure
; environmant for recursive calls of it.
;
; > Env #2 env-uid-2 factorial (n)
; ~> Frame [0 of 4]
;    n .... 1
; ~> Frame [1 of 4]
;    n .... 2
; ~> Frame [2 of 4]
;    n .... 3
; ~> Frame [3 of 4]
;    n .... 4
;
; > Env #1 env-uid-1 eval-private-scope
; ~> Frame [0 of 1]
;    factorial .... #<compound-procedure ( n )>
;

; Basic evaluator: basic lambda
(assert-eq? 5
 (eval-basic
  (define sum (lambda (a b) (+ a b)))
  (sum 2 3)
 )
)

; Basic evaluator: lambda clojure
(assert-eq? 5
 (eval-basic
  (define (make-sum a)
   (lambda (b) (+ a b))
  )

  (define sum (make-sum 2))
  (sum 3)
 )
)

; Basic evaluator: assignment
(assert-eq? 10
 (eval-basic
  (define (make-acc n)
   (lambda (v) (set! n (+ n v)))
  )

  (define acc (make-acc 0))
  (acc 2) (acc 3) (acc 5) ;<— set! returns the value
 )
)

; Basic evaluator: begin
(assert-eq? 10
 (eval-basic
  (define sum 0)
  (begin
   (set! sum 4)
   (+ sum 6)
  )
 )
)

; Basic evaluator: condition
(assert-eq? 3
 (eval-basic
  (define point 0)

  (define (next case)
   (cond
    ((= 0 case) (set! point 1))
    ((= 1 case) (set! point 2))
    (else (set! point 3))
   )
   (if (< point 3) (next point))
  )

  (next 0)
  point ;<— expected result 3
 )
)

; Basic evaluator: apply form
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

; Basic evaluator: variable length arguments
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

; Basic evaluator: variable length arguments for lambdas
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

; Basic evaluator: global definition with clojure
(eval-basic
 (define my-value 'Abc)
 (define (get-value) my-value)
 (global Global get-value)
)

(assert-equal? 'Abc (eval-basic (Global)))

; Standard library: mapping with primitive function
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

; Standard library: mapping with defined function
(assert-equal? '(111 222 333)
 (eval-basic
  (define (sum . args) (apply + args))
  (map sum '(100 200 300) '(10 20 30) '(1 2 3))
 )
)

; Standard library: mapping with lambda
(assert-equal? '(111 222 333)
 (eval-basic
  (map
   (lambda (. args) (apply + args))
   '(100 200 300) '(10 20 30) '(1 2 3)
  )
 )
)

; Standard library: for each
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

(log "\n" "Evaluator successfully tested!" "\n")
