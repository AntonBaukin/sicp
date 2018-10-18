(define (log . args) (for-each display args) (newline))

(define (repeated n f)
 (define (iter i v)
  (if (= i 0) v
   (iter (- i 1) (f v)))
 )

 (lambda (x) (iter n x))
)

(define (smooth dx f)
 (lambda (x)
  (*
   .3333333333333333
   (+
    (f (- x dx))
    (f x)
    (f (+ x dx))
   )
  )
 )
)

(define (smooth-nth n dx f)
 (repeated n (smooth dx f))
)

(define (mod-sample i)
 (let
  (
   (i5 (quotient (round i) 5))
   (r5 (remainder (round i) 5))
  )
  (if (even? i5)
   (+ 1 r5)
   (- 4 r5)
  )
 )
)

(define (print-func a b f)
 (define (match? j i)
  (> (f i) j)
 )

 (define (iter j i)
  (let ((m? (match? j i)))
   (display (if m? "x" " "))
   (if (> i b) #f (or (iter j (+ i 1)) m?))
  )
 )

 (define (print-row j)
  (let ((row? (iter j a)))
   (newline)
   (if row? (print-row (+ j 1)) #f)
  )
 )

 (print-row 0)
)

(print-func 0 48 mod-sample)
(print-func 0 48 (smooth-nth 1 1 mod-sample))
(print-func 0 48 (smooth-nth 2 1 mod-sample))
(print-func 0 48 (smooth-nth 3 1 mod-sample))
(print-func 0 48 (smooth-nth 4 1 mod-sample))
