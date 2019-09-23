
(define (make-polynomial-ops reduce-terms)
 (include "2.5.3-iterate-two.scm")


 ; In our general arithmetics we have no negate operator,
 ; but with the types tower and auto-raise we can define
 ; it as (integer 0 – general value).
 (define (negate v) (sub 0 v))

 ; Identity unary function, returns the same term
 (define (no-op v) v)
 
 ; General operation on two terms of the same order.
 ; Invoked with coefficients of the terms.
 (define (term-bi-op op a b)
  (cons (car a) (op (cdr a) (cdr b)))
 )

 (define (term-uni-op op a)
  (cons (car a) (op (cdr a)))
 )

 ; Unlike SICP course, we define linear ops on
 ; terms as special merge on two sorted sets.
 (define (linear-terms-op terms-proc terms-a terms-b)
  (reduce-terms (reverse (terms-proc terms-a terms-b)))
 )

 ; Terms processing is not a mere merge. For addition,
 ; this is true: we directly take terms on orders mismatch,
 ; and summarize them on match. But for subtraction this
 ; is not true: we take first argument as-is, but negate
 ; the second one.
 (define (make-terms-proc bi-op a-op b-op)

  ; On oders mismatch, we pop the highest term (as the
  ; sparse terms set order is power descending), or we
  ; pop them both merging them.
  (define (where a b w t)
   (cond
    ((< (caar a) (caar b)) #t) ;<— pop higher b-term
    ((< (caar b) (caar a)) #f) ;<— pop higher a-term
    (else (bi-op (car a) (car b)))       ;<— pop both & merge
   )
  )

  ; Differs from merge in that we apply a-op or b-op
  ; before adding to the result t-list.
  (define (take a b w t)
   (cond
    ((eq? #f w) (cons (a-op (car a)) t))
    ((eq? #t w) (cons (b-op (car b)) t))
    (else (cons w t))
   )
  )

  (lambda (terms-a terms-b)
   (iterate-two terms-a terms-b where take)
  )
 )

 (define (bind-linear-call bi-op a-op b-op)
  (define linear-call
   (curry linear-terms-op
    (make-terms-proc bi-op a-op b-op)
   )
  )
  
  (lambda (terms-a terms-b) (linear-call terms-a terms-b))
 )

 ; As in SICP, we use general arithmetics on terms.
 (define add-bi-op (curry term-bi-op add))
 (define add-sparse-terms (bind-linear-call add-bi-op no-op no-op))
 (define sub-bi-op (curry term-bi-op sub))
 (define neg-b-op (curry term-uni-op negate))
 (define sub-sparse-terms (bind-linear-call sub-bi-op no-op neg-b-op))


 (define (mul-terms a b)
  (cons
   (+ (car a) (car b))   ;<— sum the orders
   (mul (cdr a) (cdr b)) ;<— general mul of the coefficients
  )
 )

 ; Mutiplies terms sets by single term reversing the order.
 (define (mul-terms-by-term-iter terms term res)
  (if (null? terms) res
   (mul-terms-by-term-iter (cdr terms) term
    (cons (mul-terms (car terms) term) res)
   )
  )
 )

 (define (mul-terms-by-term terms term)
  (reverse (mul-terms-by-term-iter terms term '()))
 )

 (define (mul-poly-terms-iter res terms-a terms-b)
  (if (null? terms-b) res
   (mul-poly-terms-iter
    (add-sparse-terms res
     (mul-terms-by-term terms-a (car terms-b))
    )
    terms-a (cdr terms-b)
   )
  )
 )

 (define mul-sparse-terms (curry mul-poly-terms-iter '()))


 ; Implementation of task 2.91. Returns pair of terms.
 ; The rest '() means zero — the case of full division.
 (define (div-sparse-terms terms-num terms-den)
  (if (null? terms-num) '()
   (let (
     (onum (caar terms-num))
     (oden (caar terms-den))
    )

    (if (> oden onum)
     (cons '() terms-num)
     (let* (
       (cnum (cdar terms-num))
       (cden (cdar terms-den))
       (cnew (div cnum cden))
       (onew (- onum oden))
       (tnew (cons onew cnew)) ;<— new resulting term
       (mult (mul-terms-by-term terms-den tnew))
       (diff (sub-sparse-terms terms-num mult))
       (next (div-sparse-terms diff terms-den))
      )

      (cons
       ; We push the new term to be the first in the recursive
       ; result quotient thus forming the desired descending order.
       (cons tnew (if (null? next) '() (car next)))
       (if (null? next) '() (cdr next)) ;<— the rest stays as is
      )
     )
    )
   )
  )
 )


 (list ; Resulting functions:
  add-sparse-terms
  sub-sparse-terms
  mul-sparse-terms
  div-sparse-terms
 )
)