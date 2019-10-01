(define (log . args) (for-each display args) (newline))

(define nil '())

; (good-enough? i Xi <prev Xi or nil>)
(define (iterative-improve good-enough? improve)
 (define (next i Xi Xi_)
  (if (good-enough? i Xi Xi_) Xi
   (next (+ i 1) (improve Xi) Xi)
  )
 )

 (lambda (x0) (next 0 x0 nil))
)

(define precision 0.0001)

(define (sqrt-newton x)
 (define (good-enough? i y _)
  (or
   (> i 100)
   (< (abs (- (* y y) x)) precision)
  )
 )

 (define (improve y)
  (* 0.5 (+ y (/ x y)))
 )

 ((iterative-improve good-enough? improve) (* 0.1 x))
)

(define (fixed-point x0 f)
 (define (good-enough? i y y_)
  (or
   (> i 100)
   (and
    (> i 0)
    (< (abs (- y y_)) precision)
   )
  )
 )

 ((iterative-improve good-enough? f) x0)
)

(define (dump-avg f)
 (lambda (x) (* 0.5 (+ x (f x))))
)

(define (sqrt-fixpoint x)
 (fixed-point (* 0.1 x) (dump-avg (lambda (y) (/ x y))))
)

(log "     newton √121 = " (sqrt-newton 121))
(log "fixed point √121 = " (sqrt-fixpoint 121))
(log "     newton √2   = " (sqrt-newton 2))
(log "fixed point √2   = " (sqrt-fixpoint 2))