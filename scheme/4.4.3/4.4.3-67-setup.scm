(
 (lambda () ;<— immediately invoked function
  (define impl (make-qeval-loop-detector 20 5))
  (set! qeval-disp-protect (make-qeval-protector (car impl) (cdr impl)))
 )
)
