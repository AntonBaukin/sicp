(define (log . args) (for-each display args) (newline))

(define (vector-make x y)
 (cons x y)
)

(define (vector-x v)
 (car v)
)

(define (vector-y v)
 (cdr v)
)

(define (vector-str v)
 (string-append
  "("
  (number->string (vector-x v))
  " "
  (number->string (vector-y v))
  ")"
 )
)

(define (vector-length v)
 (sqrt
  (+
   (square (vector-x v))
   (square (vector-y v))
  )
 )
)

(define (vector-add a b)
 (vector-make
  (+ (vector-x a) (vector-x b))
  (+ (vector-y a) (vector-y b))
 )
)

(define (vector-neg v)
 (vector-make (- (vector-x v)) (- (vector-y v)))
)

(define (vector-sub a b)
 (vector-add a (vector-neg b))
)

; Creates ortogonal vector to v of the length l going
; clock-wise for positive l, and opposite for negative.
(define (vector-ortogonal v l)
 (let
  (
   (bx (/ (* (vector-y v) (abs l)) (vector-length v)))
   (by (/ (* (vector-x v) (abs l)) (vector-length v)))
  )

  (if (> l 0)
   (vector-make bx (- by))
   (vector-make (- bx) by)
  )
 )
)

; General perimiter of a rectangle
(define (rect-perimeter r get-length-a get-length-b)
 (+ (* 2 (get-length-a r)) (* 2 (get-length-b r)))
)

; General area of a rectangle
(define (rect-area r get-length-a get-length-b)
 (* (get-length-a r) (get-length-b r))
)

; Creates rectangle with a corner located at the coordinates
; center O, vector A defines one side (direction and length) and
; goes out of the center O, else side B is defined by it's length:
; positive value goes clock-wise, negative is counter.
(define (rect-1-make veca b)
 (cons veca b)
)

; The length of side A.
(define (rect-1-length-a r)
 (sqrt
  (+
   (square (vector-x (car r)))
   (square (vector-y (car r)))
  )
 )
)

; The length of side B.
(define (rect-1-length-b r)
 (abs (cdr r))
)

(define (rect-1-perimeter r)
 (rect-perimeter r rect-1-length-a rect-1-length-b)
)

(define (rect-1-area r)
 (rect-area r rect-1-length-a rect-1-length-b)
)

; Vectors of 4 points: O, A, B, C, where O is the initial
; point, here it's always a coordinates center as we do not
; move the rectangle across the XY-plane.
(define (rect-1-vector-o r)
 (vector-make 0 0)
)

(define (rect-1-vector-a r)
 (car r)
)

(define (rect-1-vector-b r)
 (vector-add (car r) (vector-ortogonal (car r) (cdr r)))
)

(define (rect-1-vector-c r)
 (vector-add (rect-1-vector-o r) (vector-ortogonal (car r) (cdr r)))
)

(define test-rect-1
 (rect-1-make (vector-make 3 2)
  (- (* 2 (sqrt 13))) ; B — 2x length, counter clock
 )
)

(log "rect-1 point A " (vector-str (rect-1-vector-a test-rect-1)))
(log "rect-1 point B " (vector-str (rect-1-vector-b test-rect-1)))
(log "rect-1 point C " (vector-str (rect-1-vector-c test-rect-1)))
(log "rect-1 perim   " (rect-1-perimeter test-rect-1))
(log "rect-1 area    " (rect-1-area test-rect-1))


; Creates rectangle starting from coordinates center O;
; side A goes out of it by the angle α counter-clock;
; then goes side B, counter-clock also. α is in radians.
(define (rect-2-make a b α)
 (cons (cons a b) α)
)

; The length of side A.
(define (rect-2-length-a r)
 (caar r)
)

; The length of side B.
(define (rect-2-length-b r)
 (cdar r)
)

(define (rect-2-perimeter r)
 (rect-perimeter r rect-2-length-a rect-2-length-b)
)

(define (rect-2-area r)
 (rect-area r rect-2-length-a rect-2-length-b)
)

(define (rect-2-vector-o r)
 (vector-make 0 0)
)

(define (rect-2-vector-a r)
 (vector-make
  (* (rect-2-length-a r) (cos (cdr r)))
  (* (rect-2-length-a r) (sin (cdr r)))
 )
)

(define (rect-2-vector-b r)
 (let
  ((a (rect-2-vector-a r)))
  (vector-add a (vector-ortogonal a (- (rect-2-length-b r))))
 )
)

(define (rect-2-vector-c r)
 (vector-add
  (rect-1-vector-o r)
  (vector-ortogonal (rect-2-vector-a r) (- (rect-2-length-b r)))
 )
)

; Same rect as test-rect-1 in second form.
(define test-rect-2
 (rect-2-make (sqrt 13) (* 2 (sqrt 13)) 0.588002603547568)
)

(newline)
(log "rect-2 point   A " (vector-str (rect-2-vector-a test-rect-2)))
(log "rect-2 point   B " (vector-str (rect-2-vector-b test-rect-2)))
(log "rect-2 point   C " (vector-str (rect-2-vector-c test-rect-2)))
(log "rect-2 perimeter " (rect-2-perimeter test-rect-2))
(log "rect-2 area      " (rect-2-area test-rect-2))
