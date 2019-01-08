(define (log . args) (for-each display args) (newline))

(define nil (list))

(define (square-list-1 items)
 (if (null? items) nil
  (cons
   (square (car items))
   (square-list-1 (cdr items))
  )
 )
)

(define (square-list-2 items)
 (map square items)
)

(log "1: square of (1 2 3 4) is " (square-list-1 (list 1 2 3 4)))
(log "2: square of (1 2 3 4) is " (square-list-2 (list 1 2 3 4)))
