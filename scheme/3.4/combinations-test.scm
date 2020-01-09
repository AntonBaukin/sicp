(include "../3.3.2/assert.scm")
(include "combinations.scm")

(define (log . args) (for-each display args))


(define (factorial n)
 (if (= 1 n) 1 (* n (factorial (- n 1))))
)

(define (log-permutations items)
 (define ps (permutations items))

 (assert-eq? (length ps) (factorial (length items)))
 (log "Permutating " items " [" (length ps) "]: ")
 (for-each (lambda (p) (log p " ")) ps)
 (log "\n\n")
)

(define (log-order-shuffle items)
 (define os (order-shuffle items))

 (log "Order shuffle " items " [" (length os) "]: ")
 (for-each (lambda (p) (log p " ")) os)
 (log "\n\n")
)

(log-permutations '(1 2 3))
(log-permutations '(1 2 3 4))
(log-permutations '(1 2 3 4 5))

(log-order-shuffle '((a b) (x)))
(log-order-shuffle '((a b) (x y)))
(log-order-shuffle '((a b c) (x y z)))
