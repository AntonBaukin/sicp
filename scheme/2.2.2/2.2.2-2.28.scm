(define (log . args) (for-each display args) (newline))

(define nil (list))

(define (fringe tree)

 (define (next res tail)
  (if (null? tail) res
   (if (pair? (car tail))
    (append res (fringe (car tail)) (fringe (cdr tail)))
    (next (append res (list (car tail))) (cdr tail))
   )
  )
 )

 (next nil tree)
)

(define a (list (list 1 2) (list 3 4)))
(define b (list (list 5 6) (list 7 8 9)))

(log "fringe " a " —is— " (fringe a))
(log "fringe " (list a b) " —is— " (fringe (list a b)))
