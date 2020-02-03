
; When precision is positive, prints fixed point
; number having fraction digits padded with zeros.
; Negative precision means up-to fraction digits:
; trailing zeros are omitted.
(define (make-float->str precision)
 (define (pad i tail res)
  (cond
   ((>= i precision) res)
   ((null? tail) (pad (+ i 1) '() (cons #\0 res)))
   (else (pad (+ i 1) (cdr tail) (cons (car tail) res)))
  )
 )

 (define (trim l)
  (cond
   ((null? l) (list #\0))
   ((eq? #\0 (car l)) (trim (cdr l)))
   (else l)
  )
 )

 (define (rpad r)
  (define l (string->list (number->string r)))
  (list->string
   (if (>= precision 0)
    (reverse (pad 0 l '()))
    (reverse (trim (reverse l)))
   )
  )
 )

 (lambda (float)
  (define s (if (< float 0) "-" "+"))
  (define i (exact (truncate (abs float))))
  (define r (exact (truncate (* (expt 10 (abs precision)) (- (abs float) i)))))
  (string-append s (number->string i) "." (rpad r))
 )
)

(define (float->str precision float)
 ((make-float->str precision) float)
)
