
(define (make-sparse-polynomial-cut)
 (define make-poly (list-ref polynomial-package 0))

 (define (remainder-terms a b)
  (let* (
    (pa (make-poly '? a))
    (pb (make-poly '? b))
    ; Division result from 2.91 already has the remainder
    ; as the second part of the returned pair:
    (ir (div pa pb))
   )
   ; Here we unwrap sparse terms:
   (apply-generic-unwrap
    ; Here we unwrap phony polynomial:
    (cdr (apply-generic-unwrap (cdr ir)))
   )
  )
 )

 (define (div-terms a b)
  (let* (
    (pa (make-poly '? a))
    (pb (make-poly '? b))
    (ir (div pa pb))
   )
   ; Here we unwrap sparse terms:
   (apply-generic-unwrap
    ; Here we unwrap phony polynomial:
    (cdr (apply-generic-unwrap (car ir)))
   )
  )
 )

 (define (gcd-terms a b)
  (if (null? b) a
   (gcd-terms b (remainder-terms a b))
  )
 )

 ; Checks whether the terms are of polynomial with
 ; a term order being not zero.
 (define (terms-poly? terms)
  (and
   (not (null? terms))
   (> (caar terms) 0)
  )
 )

 (define (cut-terms terms-a terms-b)
  (let ((g (gcd-terms terms-a terms-b)))
   (if (not (terms-poly? g))
    (cons terms-a terms-b) ;<— do not divide them
    (cons
     (div-terms terms-a g)
     (div-terms terms-b g)
    )
   )
  )
 )

 (list ;<— collection of resulting functions
  cut-terms
  div-terms
  remainder-terms
  gcd-terms
  terms-poly?
 )
)
