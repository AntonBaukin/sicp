(include "2.1.4-2.x.scm")

(define (interval-center i)
 (* 0.5 (+ (interval-low i) (interval-up i)))
)

(define (interval-radius i)
 (* 0.5 (- (interval-up i) (interval-low i)))
)

(define (interval-make-% center percent)
 (let
  ((r (* 0.01 percent center)))
  (interval-make (- center r) (+ center r))
 )
)

(define (interval-% i)
 (* 100 (/ (interval-radius i) (interval-center i)))
)

(define (interval-str-% i)
 (string-append
  "["
  (number->string (interval-center i))
  " ± "
  (number->string (* 0.01 (round (* 100 (interval-% i)))))
  "%]"
 )
)
