(define (log . args) (for-each display args) (newline))

(define (accumulate sequence initial op)
 (define (recurse tail)
  (if (null? tail) initial
   (op (car tail) (recurse (cdr tail)))
  )
 )

 (recurse sequence)
)

(log "sum of (1 2 3 4) = " (accumulate (list 1 2 3 4) 0 +))

(define (map sequence op)
 (accumulate sequence (list)
  (lambda (v acc) (cons (op v) acc))
 )
)

(log "squares of (2 3 4 5) = " (map (list 2 3 4 5) square))

(define (append seqa seqb)
 (accumulate seqa seqb cons)
)

(log "(1 2 3) + (4 5 6) = " (append (list 1 2 3) (list 4 5 6)))

(define (length sequence)
 (accumulate sequence 0 (lambda (v acc) (+ acc 1)))
)

(log "length of (1 2 3 4 5) = " (length (list 1 2 3 4 5)))
