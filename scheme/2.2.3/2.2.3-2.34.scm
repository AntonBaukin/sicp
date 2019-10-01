(define (log . args) (for-each display args) (newline))

(define (accumulate sequence initial op)
 (define (recurse tail)
  (if (null? tail) initial
   (op (car tail) (recurse (cdr tail)))
  )
 )

 (recurse sequence)
)

(define (horner-eval x coeffs)
 (accumulate coeffs 0 (lambda (this-coeff higher-terms)
   (+ (* higher-terms x) this-coeff)
 ))
)

(define (accumulate-i sequence initial op)
 (define (recurse i tail)
  (if (null? tail) initial
   (op i (car tail) (recurse (+ i 1) (cdr tail)))
  )
 )

 (recurse 0 sequence)
)

(define (plain-eval x coeffs)
 (accumulate-i coeffs 0 (lambda (i Ai sum)
  (+ sum (* Ai (expt x i)))
 ))
)

(log " x = 2 of 1 + 3x + 5x^3 + x^5 horner eval = "
 (horner-eval 2 (list 1 3 0 5 0 1))
 " plain eval = "
 (plain-eval 2 (list 1 3 0 5 0 1))
)
