(include "2.1.4-2.x.scm")

(define (interval-radius i)
 (* 0.5 (- (interval-up i) (interval-low i)))
)

(log "radius [1 .. 2] = " (interval-radius (interval-make 1 2)))

(log "radius [1 .. 2] + [1 .. 1.5] = " (interval-radius
 (interval-add (interval-make 1 2) (interval-make 1 1.5))))

(log "radius [1 .. 2] + radius [1 .. 1.5] = " (+
 (interval-radius (interval-make 1 2))
 (interval-radius (interval-make 1 1.5))
))

(log "radius [1 .. 2] * [1 .. 1.5] = " (interval-radius
 (interval-mul (interval-make 1 2) (interval-make 1 1.5))))

(log "radius [1 .. 2] * radius [1 .. 1.5] = " (*
 (interval-radius (interval-make 1 2))
 (interval-radius (interval-make 1 1.5))
))
