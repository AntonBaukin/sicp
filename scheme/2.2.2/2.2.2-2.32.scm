(define (log . args) (for-each display args) (newline))

(define (subsets set)
 (if (null? set)
  (list (list))
  (let
   ((rest (subsets (cdr set))))
   (append rest (map (lambda (s) (cons (car set) s)) rest))
  )
 )
)

(log "subsets of (1 2 3):" "\n" (subsets (list 1 2 3)))
(newline)
(log "subsets of (1 2 3 4):" "\n" (subsets (list 1 2 3 4)))
(newline)
(log "subsets of (1 2 3 4 5):" "\n" (subsets (list 1 2 3 4 5)))
