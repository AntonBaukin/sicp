(define (log . args) (for-each display args) (newline))

(define (point-make x y)
 (cons x y)
)

(define (point-x p)
 (car p)
)

(define (point-y p)
 (cdr p)
)

(define (point-str p)
 (string-append
  "("
  (number->string (point-x p))
  " "
  (number->string (point-y p))
  ")"
 )
)

;(log (point-str (point-make 1.0 2.5)))

(define (segment-make a b)
 (cons a b)
)

(define (segment-make-4 ax ay bx by)
 (segment-make (point-make ax ay) (point-make bx by))
)

(define (segment-a s)
 (car s)
)

(define (segment-b s)
 (cdr s)
)

(define (segment-str s)
 (string-append
  (point-str (segment-a s))
  "–"
  (point-str (segment-b s))
 )
)

;(log (segment-str (segment-make-4 1.0 1.5 2.2 3.0)))

(define (segment-midpoint s)
 (define (mid sel)
  (* 0.5
   (+
    (sel (segment-a s))
    (sel (segment-b s))
   )
  )
 )

 (point-make (mid point-x) (mid point-y))
)

(define (test-segment-midpoint s)
 (log (segment-str s) " middle is "
  (point-str (segment-midpoint s))
 )
)

(test-segment-midpoint (segment-make-4 1.0 2.0 2.0 3.0))
