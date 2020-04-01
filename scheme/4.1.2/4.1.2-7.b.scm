(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

; Here we implement «let*» form directly via «let».
; For «register» see «4.1.2-4.a.scm».
(eval-basic (register
 let ;<— we do not to quote it

 (lambda (exp env)
  (define (let->lambda clauses body)
   (append
    (list
     (cons
      'lambda
      (cons
       (map car clauses)
       body
      )
     )
    )
    (map cadr clauses)
   )
  )

  (eval (let->lambda (cadr exp) (cddr exp)))
 )

 let* ;<— we do not to quote it

 (lambda (exp env)

  (define (let*->let clauses body)
   (list
     'let
    (list
     (list (caar clauses) (cadar clauses))
    )

    (if (null? (cdr clauses))
     (if (= 1 (length body))
      (car body)
      (append '(begin) body)
     )
     (let*->let (cdr clauses) body)
    )
   )
  )

  (debug log (let*->let (cadr exp) (cddr exp)))
  (eval (let*->let (cadr exp) (cddr exp)))
 )
))

(assert-eq? 123
 (eval-basic
  (let* (
    (a 100)
    ;(b (/ a 5))
    ;(c (/ (+ a b) -40))
   )
   ;(set! c (- c))
   ;(+ a b)
   (- a)
  )
 )
)
