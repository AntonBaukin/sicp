(define (log . args) (for-each display args) (newline))

; This tricky function overwrites itself on the first call.
(define (f x)
 (set! f (lambda (y) (* y x)))
 x
)

(log "f(0) = " (f 0))
(log "f(1) = " (f 1))


(define (f x)
 (set! f (lambda (y) (* y x)))
 x
)

(newline)
(log "f(1) = " (f 1))
(log "f(0) = " (f 0))


(define (f x)
 (set! f (lambda (y) (* y x)))
 x
)

(newline)
(log "(+ (f 0) (f 1)) = " (+ (f 0) (f 1)))


(define (f x)
 (set! f (lambda (y) (* y x)))
 x
)

(log "(+ (f 1) (f 0)) = " (+ (f 1) (f 0)))
