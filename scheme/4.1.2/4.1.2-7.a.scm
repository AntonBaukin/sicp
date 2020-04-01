(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

; Here we implement «let*» form directly via «if».
; For «register» see «4.1.2-4.a.scm».
(eval-basic (register
 let* ;<— we do not to quote it

 (lambda (exp env)
  (define (let*->lambda clauses body)
   (define (wrap-begin exp)
    (if (= 1 (length exp))
     (car exp)
     (append '(begin) exp)
    )
   )

   ; Dynamic evaluation is described in the previous task, 4.1.2-6.
   (define (make-eval-dynamic exp)
    (list 'eval-dynamic 'env (wrap-begin exp))
   )

   (append
    (list
     (cons
      'lambda
      (list
       (list (caar clauses))
       (if (null? (cdr clauses))
        (make-eval-dynamic body)
        (let*->lambda (cdr clauses) body)
       )
      )
     )
    )

    (list (make-eval-dynamic (cdar clauses)))
   )
  )

  ;(debug log "LET* " exp " >> " (let*->lambda (cadr exp) (cddr exp)))
  (eval (let*->lambda (cadr exp) (cddr exp)))
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
