(include "2.3.2-2.5x.scm")

(define (=number? num expr)
 (and (number? expr) (= expr num))
)

(define (make-sum expr-a expr-b)
 (cond
  ((=number? 0 expr-a) expr-b)
  ((=number? 0 expr-b) expr-a)

  ((and (number? expr-a) (number? expr-b))
   (+ expr-a expr-b)
  )

  (else (list '+ expr-a expr-b))
 )
)

(define (make-product expr-a expr-b)
 (cond
  ((=number? 0 expr-a) 0)
  ((=number? 0 expr-b) 0)
  ((=number? 1 expr-a) expr-b)
  ((=number? 1 expr-b) expr-a)

  ((and (number? expr-a) (number? expr-b))
   (* expr-a expr-b)
  )

  (else (list '* expr-a expr-b))
 )
)
