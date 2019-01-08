(define nil (list))

; note the reversed order of the arguments!
(define (for-eachx args proc)
 (define (ignore x) nil)
 (define (step tail ignore) (cdr tail))

 (define (iter tail)
  (if (null? tail) nil
   (iter (step tail (proc (car tail))))
  )
 )

 (iter args)
)

(for-eachx (list 57 321 88)
 (lambda (x) (display x) (newline))
)
