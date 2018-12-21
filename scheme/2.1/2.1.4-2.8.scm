(include "2.1.4-2.x.scm")

(define (interval-sub i j)
 (interval-make
  (- (interval-low i) (interval-up j))
  (- (interval-up i) (interval-low j))
 )
)

(log "[3 .. 5] - [1 .. 2] = " (interval-str
  (interval-sub (interval-make 3 5) (interval-make 1 2))))
