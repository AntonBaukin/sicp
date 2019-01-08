(include "2.1.4-2.x.scm")

(define (interval-includes? x i)
 (not
  (or
   (< x (interval-low i))
   (> x (interval-up i))
  )
 )
)

(define (interval-xzero? i)
 (interval-includes? 0 i)
)

(define (interval-div-safe i j)
 (if (interval-xzero? j)
  (raise
   (string-append "Dividing interval "
    (interval-str j) " crosses zero!" )
  )
  (interval-div i j)
 )
)

(log "[2 .. 3] / [-2 .. 2] = " (interval-str
  (interval-div (interval-make 2 3) (interval-make -1 1))))

(log "[2 .. 3] / [-0.25 .. 10] = " (interval-str
  (interval-div (interval-make 2 4) (interval-make -0.2 10))))

(log "[2 .. 3] includes 1 ?= " (interval-includes?
  1 (interval-make 2 3)
))

(log "[2 .. 3] includes 2.5 ?= " (interval-includes?
  2.5 (interval-make 2 3)
))

(log "[-1 .. 1] crosses 0 ?= " (interval-xzero? (interval-make -1 1)))

(log "safe [2 .. 3] / [-2 .. 2] = ...")
(interval-str (interval-div-safe (interval-make 2 3) (interval-make -2 2)))