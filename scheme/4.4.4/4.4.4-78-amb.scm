(include "../4.3.1/eval-amb.scm")

(eval-basic
 (define (require p)
  (if (not p) (amb))
 )

 (global require)

 (define (amb-of items)
  (require (not (null? items)))
  (amb (car items) (amb-of (cdr items)))
 )

 (global amb-of)
)

