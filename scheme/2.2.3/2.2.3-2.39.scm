(define (log . args) (for-each display args) (newline))

; operation is (op v acc)
(define (fold-right sequence initial op)
 (define (recurse tail)
  (if (null? tail) initial
   (op (car tail) (recurse (cdr tail)))
  )
 )

 (recurse sequence)
)

; operation is (op v acc)
(define (fold-left sequence initial op)
 (define (iter res tail)
  (if (null? tail) res
   (iter (op (car tail) res) (cdr tail))
  )
 )

 (iter initial sequence)
)

(define (reverse-right sequence)
 (fold-right sequence (list) (lambda (v acc) (append acc (list v))))
)

(define (reverse-left sequence)
 (fold-left sequence (list) cons)
)

(log "reverse right " (reverse-right (list 1 2 3 4 5)))
(log "reverse  left " (reverse-left (list 1 2 3 4 5)))
