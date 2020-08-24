
(eval-basic
 (define (require p)
  (if (not p) (amb))
 )

 (global require)
)
