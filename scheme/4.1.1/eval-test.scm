(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-basic.scm")

; This simplest form just returns the evaluated expression.
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

; Basic evaluator: inner definition.
;(assert-eq? 'XYZ (eval-basic (define xyz 'XYZ) xyz))

; Basic evaluator: local scope of inner definitions.
;(assert-error (lambda () (eval-basic xyz)) void)
