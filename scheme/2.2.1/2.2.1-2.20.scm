(define (log . args) (for-each display args) (newline))

(define (same-parity v . nums)
 (define (same? x)
  (= (remainder v 2) (remainder x 2))
 )

 (define (collect res tail)
  (if (null? tail) res
   (if (same? (car tail))
    (collect (cons (car tail) res) (cdr tail))
    (collect res (cdr tail))
   )
  )
 )

 (reverse (collect (list v) nums))
)

(log "same parity of (1 2 3 4 5 6 7) is "
 (same-parity 1 2 3 4 5 6 7)
)

(log "same parity of (2 3 4 5 6 7) is "
 (same-parity 2 3 4 5 6 7)
)
