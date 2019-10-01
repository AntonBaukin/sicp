(define (log . args) (for-each display args) (newline))

(define (repeated n f)
 (define (iter i v)
  (if (= i 0) v
   (iter (- i 1) (f v)))
 )

 (lambda (x) (iter n x))
)

(log "((repeated 2 square) 5) = " ((repeated 2 square) 5))
(log "((repeated 2 square) 5) = " ((repeated 3 square) 5))
