(define (log . args) (for-each display args) (newline))

(define (make-vect x y)
 (cons x y)
)

(define (xcor-vect v)
 (car v)
)

(define (ycor-vect v)
 (cdr v)
)

(define (vect->str v)
 (string-append
  "("
  (number->string (xcor-vect v))
  " "
  (number->string (ycor-vect v))
  ")"
 )
)

(define (make-frame origin edge1 edge2)
 (list origin edge1 edge2)
)

(define (origin-frame f)
 (car f)
)

(define (edge1-frame f)
 (cadr f)
)

(define (edge2-frame f)
 (caddr f)
)

(define (frame->str f)
 (string-append
  "["
  (vect->str (origin-frame f))
  " "
  (vect->str (edge1-frame f))
  " "
  (vect->str (edge2-frame f))
  "]"
 )
)

(define V0 (make-vect 0.0 0.0))
(define E1 (make-vect 1.0 0.0))
(define E2 (make-vect 0.0 1.0))

(log "frame v1 " (frame->str (make-frame V0 E1 E2)))


; —————————————————————————————————————————————————


(define (make-frame origin edge1 edge2)
 (cons origin (cons edge1 edge2))
)

(define (edge2-frame f)
 (cddr f)
)

(log "frame v2 " (frame->str (make-frame V0 E1 E2)))