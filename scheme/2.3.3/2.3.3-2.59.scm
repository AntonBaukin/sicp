(define (log . args) (for-each display args) (newline))

(define (element-of-set? set x)
 (if (null? set) #f
  (if (equal? x (car set)) #t
   (element-of-set? (cdr set) x)
  )
 )
)

(define (union-set seta setb)
 (if (null? seta) setb
  (if (element-of-set? setb (car seta))
   (union-set (cdr seta) setb)
   (cons (car seta) (union-set (cdr seta) setb))
  )
 )
)

(define (intersection-set seta setb)
 (if (or (null? seta) (null? setb)) '()
  (if (element-of-set? setb (car seta))
   (cons (car seta) (intersection-set (cdr seta) setb))
   (intersection-set (cdr seta) setb)
  )
 )
)

(log "(1 2 3 4) ∪ (2 4 6 8) = " (union-set '(1 2 3 4) '(2 4 6 8)))
(log "(1 2 3 4) ∩ (2 4 6 8) = " (intersection-set '(1 2 3 4) '(2 4 6 8)))
