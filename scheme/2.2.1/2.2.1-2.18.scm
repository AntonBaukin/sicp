(define (log . args) (for-each display args) (newline))

(define nil (list))

(define (reverse l)
 (define (next res tail)
  (if (null? tail) res
   (next (cons (car tail) res) (cdr tail))
  )
 )

 (next nil l)
)

(log "reverse of (1 2 3 4 5) is " (reverse (list 1 2 3 4 5)))
