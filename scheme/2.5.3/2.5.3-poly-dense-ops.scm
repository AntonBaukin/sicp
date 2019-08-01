
; Operations for polynomials with dense terms list.
(define (make-poly-dense-ops)

 ; Takes two lists of coefficients and merges
 ; them with the operation given. As you may
 ; guess, it's more like merge-sorted() used
 ; in linear-terms-op() for sparse terms.
 ; Returns coeffs in reverse order!
 ;
 (define (linear-coeffs-op-iter op coeffs-a coeffs-b res)
  (cond
   ((and (null? coeffs-a) (null? coeffs-b))
    res
   )

   ((null? coeffs-a)
    (linear-coeffs-op-iter op coeffs-a (cdr coeffs-b)
     (cons (car coeffs-b) res)
    )
   )

   ((null? coeffs-b)
    (linear-coeffs-op-iter op (cdr coeffs-a) coeffs-b
     (cons (car coeffs-a) res)
    )
   )

   (else
    (linear-coeffs-op-iter op (cdr coeffs-a) (cdr coeffs-b)
     (cons (op (car coeffs-a) (car coeffs-b)) res)
    )
   )
  )
 )

 ; Takes coeffs in reverse order (power descending) and
 ; returns list in normal order (power ascending from 0)
 ; having the higher zeros trimmed.
 (define (reduce-coeffs-iter coeffs res)
  (if (null? coeffs) res
   (if (zero? (car coeffs))
    (reduce-coeffs-iter (cdr coeffs) res)
    (append res (reverse coeffs)) ;<— break iteration
   )
  )
 )

 (define (linear-coeffs-op op coeffs-a coeffs-b)
  (reduce-coeffs-iter
   (linear-coeffs-op-iter op coeffs-a coeffs-b '())
   '()
  )
 )

 (define (bind-linear-call op)
  (define coeffs-call (curry linear-coeffs-op op))
  (lambda (a b) (coeffs-call a b))
 )

 ; We use general arithmetics on the cefficients.
 (define add-dense-terms (bind-linear-call add))
 (define sub-dense-terms (bind-linear-call sub))


 ; Resulting functions:
 (list add-dense-terms sub-dense-terms)
)
