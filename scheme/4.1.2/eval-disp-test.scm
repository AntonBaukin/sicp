(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")
(include "../4.1.1/eval-test-items.scm")

; Dispatching evaluator: plain eval
(assert-eq? 123
 (eval-basic (eval '123))
)

; Dispatching evaluator: eval with call
(assert-eq? 123
 (eval-basic (eval '(+ 100 20 3)))
)

; Dispatching evaluator: eval with variable
(assert-eq? 123
 (eval-basic
  (define twenty 20)
  (eval '(+ 100 twenty 3))
 )
)

; Dispatching evaluator: eval with local variable
(assert-eq? 123
 (eval-basic
  (define (inner)
   ; See, how cute our evaluator! In Gambit Schmeme it may
   ; access only global scope, but our's uses local, like
   ; JavaScript's does. This has known side-effects...
   (define twenty 20)
   (eval '(+ 100 twenty 3))
  )

  (inner)
 )
)