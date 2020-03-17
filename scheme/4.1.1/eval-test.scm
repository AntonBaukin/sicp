(define (log . args) (for-each display args) (newline))

(define basic-evaluator-debug? #t)

(include "../3.3.2/assert.scm")
(include "eval-basic.scm")

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
(assert-equal? 123.0 (eval-basic '123.0))
(assert-eq? #t (eval-basic '#t))
(assert-eq? #f (eval-basic '#f))
(assert-eq? #\A (eval-basic '#\A))

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
