(define (log . args) (for-each display args) (newline))

; Enable debug mode:
(define basic-evaluator-debug? #t)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; For «register» see «4.1.2-4.a.scm».
(eval-basic (register
 cond ;<— we do not to quote it

 (lambda (exp env)
  (define (sequence->exp seq)
   ; Warning: we can not use cond while defining it!
   ; This lambda is evaluated in the inner layer, not
   ; in the private clojure of «eval-impl-*.scm»
   ; coupled with external [underlying] Schema.
   (if (null? seq) void
    (if (null? (cdr seq))
     (car seq)
     (cons 'begin seq)
    )
   )
  )

  (define (cond-else? clause)
   (eq? 'else (car clause))
  )

  (define (cond-else clauses)
   (if (null? (cdr clauses))
    (sequence->exp (cdar clauses))
    (error ("Condition else clause is not the last" clauses))
   )
  )

  (define (cond-arrow? clause)
   (eq? '=> (cadr clause))
  )

  (define (cond-arrow clauses)
   ; IIFE (Immediately Invoked Function Expression),
   ; we use it instead of «let» form.
   (list
    (cons
     'lambda
     (cons
      '(v)
      (list
       (list
        'if 'v
        (list (caddar clauses) 'v)
        (expand-clauses (cdr clauses))
       )
      )
     )
    )

    (caar clauses) ;<— the value
   )
  )

  (define (cond-if clauses)
   (list
    'if
    (caar clauses)
    (sequence->exp (cdar clauses))
    (expand-clauses (cdr clauses))
   )
  )

  (define (expand-clauses clauses)
   (if (null? clauses) void
    (if (cond-else? (car clauses))
     (cond-else clauses)
     (if (cond-arrow? (car clauses))
      (cond-arrow clauses)
      (cond-if clauses)
     )
    )
   )
  )

  (eval (expand-clauses (cdr exp)))
 )
))

(assert-eq? 123
 (eval-basic
  (cond
   ((= 1 1) 123)
   ((= 2 2) 'abc)
  )
 )
)

(assert-eq? 'abc
 (eval-basic
  (cond
   ((= 1 2) 123)
   ((= 2 2) 'abc)
  )
 )
)

(assert-equal? "Abc"
 (eval-basic
  (cond
   ((= 1 2) 123)
   ((= 2 3) 'abc)
   (else "Abc")
  )
 )
)

(assert-eq? 2
 (eval-basic
  (cond
   ((assoc 'b '((a 1) (b 2))) => cadr)
   (else 123)
  )
 )
)

(assert-eq? 123
 (eval-basic
  (cond
   ((assoc 'c '((a 1) (b 2))) => cadr)
   (else 123)
  )
 )
)
