(include "../3.3.3/tree-red-black.scm")
(include "../2.5.3/index-tree.scm")
(include "table.scm")
(include "table-cache.scm")
(include "memoize.scm")

(define (log . args) (for-each display args) (newline))

(define (sum . args)
 (log " + " args " = " (apply + args))
 (apply + args)
)

(define (head n res tail)
 (if (or (>= 0 n) (null? tail))
  (reverse res)
  (head
   (- n 1)
   (cons (car tail) res)
   (cdr tail)
  )
 )
)

(define memf void)

; Invokes mem for the values with window size «w» going right.
(define (slide w vals)
 (if (> w (length vals)) void
  (begin
   (apply memf (head w '() vals))
   (slide w (cdr vals))
  )
 )
)

(define (log-slide w v vals)
 (if (> v (length vals)) void
  (begin
   (log "Slide " (head v '() vals))
   (slide w (head v '() vals))
   (log-slide w (+ v 1) vals)
  )
 )
)

; Test simple version with limit 5:
(log "Using cache size " 3)
(set! memf (memoize 3 sum))

(log-slide 3 4 '(1 2 3 4 5 6 7))
