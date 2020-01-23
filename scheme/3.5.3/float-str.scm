
(define (float->str precision float)
 (define s (if (< float 0) "-" "+"))
 (define i (exact (truncate (abs float))))
 (define r (- (abs float) i))

 (define (pad i tail res)
  (cond
   ((> i precision) res)
   ((null? tail) (pad (+ i 1) '() (cons #\0 res)))
   (else (pad (+ i 1) (cdr tail) (cons (car tail) res)))
  )
 )

 (define (rpad r)
  (define l (string->list (number->string r)))
  (list->string
   (reverse
    (pad 0 (if (eq? #\. (car l)) l (cdr l)) '())
   )
  )

 )

 (string-append s (number->string i) (rpad r))
)
