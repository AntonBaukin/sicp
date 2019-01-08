(define (log . args) (for-each display args) (newline))

(define nil (list))

(define (last-pair l)
 (if (null? l) l
  (if (null? (cdr l))
   (cons (car l) nil)
   (last-pair (cdr l))
  )
 )
)

(log "last-pair of (1 2 3 4 5) is " (last-pair (list 1 2 3 4 5)))
