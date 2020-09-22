
(eval-basic
 (define (require p)
  (if (not p) (amb))
 )

 (define (require-all predicate items)
  (for-each
   (lambda (item) (require (predicate item)))
   items
  )
 )

 (global require)
 (global require-all)
)
