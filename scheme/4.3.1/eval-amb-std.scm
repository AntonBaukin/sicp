
(eval-basic
 (define (require p)
  (if (not p) (amb))
 )

 (define (require-not p)
  (if p (amb))
 )

 (define (require-all predicate items)
  (for-each
   (lambda (item) (require (predicate item)))
   items
  )
 )

 (global require)
 (global require-not)
 (global require-all)
)
