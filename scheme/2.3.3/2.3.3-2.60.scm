(define (log . args) (for-each display args) (newline))

; Slower search now depends on the size of the list,
; but not the size of the set: (1 1 1 1 1 1 2) Vs (1 2).
; This also applies to (intersection-set) function.
(define (element-of-set? set x)
 (if (null? set) #f
  (if (equal? x (car set)) #t
   (element-of-set? (cdr set) x)
  )
 )
)

; Very fast o(1) addition of a single item.
; Inserting too many equal items degrades the
; performance of search and intersect ops.
; If items are rarely equal, it's fair enough.
(define (adjoin-set set x)
 (cons x set)
)

; Very fast o(n) addition of multiple items.
(define (union-set seta setb)
 (append seta setb)
)

; As a side-effect, intersection removes duplicates
; of set-B, but leaves them from set-A.
(define (intersection-set seta setb)
 (if (or (null? seta) (null? setb)) '()
  (if (element-of-set? setb (car seta))
   (cons (car seta) (intersection-set (cdr seta) setb))
   (intersection-set (cdr seta) setb)
  )
 )
)

(log "1 ∈ (8 2 4 6 4 8) ?= " (element-of-set? '(8 2 4 6 4 8) 1))
(log "1 ∈ (4 2 2 1 3 2 3 4) ?= " (element-of-set? '(4 2 2 1 3 2 3 4) 1))

(log "(8 2 4 6 4 8) ∪ (4 2 2 1 3 2 3 4) = " (union-set
  '(8 2 4 6 4 8) '(4 2 2 1 3 2 3 4)
))

(log "(8 2 4 6 4 8) ∩ (4 2 2 1 3 2 3 4) = " (intersection-set
  '(8 2 4 6 4 8) '(4 2 2 1 3 2 3 4)
))