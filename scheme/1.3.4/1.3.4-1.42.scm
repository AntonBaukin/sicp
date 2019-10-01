(define (log . args) (for-each display args) (newline))

(define (compose f g)
 (lambda (x) (f (g x)))
)

(define (inc x) (+ x 1))

(log "((compose square inc) 6) = " ((compose square inc) 6))
