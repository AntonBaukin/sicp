; Produces stream of integers [a; b], «b» — included.
(define (stream-enumerate-range a b)
 (if (> a b)
  the-empty-stream
  (cons-stream a (stream-enumerate-range (+ a 1) b))
 )
)

; Produces n-length ctream of: a, a + step, a + 2*step,..
(define (stream-enumerate-stepped-n step n a)
 (if (<= n 0)
  the-empty-stream
  (cons-stream a (stream-enumerate-stepped-n step (- n 1) (+ a step)))
 )
)

(define (integers-stream from)
 (cons-stream from (integers-stream (+ 1 from)))
)

(define (stream-of value)
 (define s (cons-stream value s))
 s ;<— resulting stream
)

(define (stream-of-list l)
 (define s (cons-stream (car l) (sol (cdr l))))

 (define (sol l)
  (if (null? l) s (cons-stream (car l) (sol (cdr l))))
 )

 s ;<— resulting stream
)

(define (produced-stream producer)
 (cons-stream
  (producer)
  (produced-stream producer)
 )
)


(define (add-streams . streams)
 (apply stream-map (cons + streams))
)

; Used for tests to overwrite meaning of addition.
(define (add-streams-with op . streams)
 (apply stream-map (cons op streams))
)

(define (mul-streams . streams)
 (apply stream-map (cons * streams))
)

(define (mul-streams-with op . streams)
 (apply stream-map (cons op streams))
)

(define (scale-stream number stream)
 (mul-streams (stream-of number) stream)
)

(define (scale-stream-with op number stream)
 (mul-streams-with op (stream-of number) stream)
)

(define (sub-streams a b)
 (apply stream-map (list - a b))
)

(define (div-streams n d)
 (apply stream-map (list / n d))
)

; Takes two infinite streams: «a» and «b», and merges them
; into a stream of pairs created by make-pair strategy.
; It takes two arguments: (a-item b-item).
;
; The order of pairs is defined by the selector that takes
; 3 streams of pairs and must return one of that streams.
;
(define (pair-streams-selected a b make-pair selector)
 (define (interleave u v w)
  (define s (selector u v w))
  (cond
   ((eq? s u)
    (cons-stream (stream-car u) (interleave (stream-cdr u) v w))
   )
   ((eq? s v)
    (cons-stream (stream-car v) (interleave u (stream-cdr v) w))
   )
   ((eq? s w)
    (cons-stream (stream-car w) (interleave u v (stream-cdr w)))
   )
   (else (error "Wrong stream selected" s))
  )
 )

 (define (pair-left l y)
  (stream-map (lambda (r) (make-pair l r)) y)
 )

 (define (pair-right x r)
  (stream-map (lambda (l) (make-pair l r)) x)
 )

 (define (nest x y)
  (cons-stream
   (make-pair (stream-car x) (stream-car y))
   (interleave
    (pair-left (stream-car x) (stream-cdr y))
    (nest (stream-cdr x) (stream-cdr y))
    (pair-right (stream-cdr x) (stream-car y))
   )
  )
 )

 (nest a b)
)

; Uses weighted selector to create infinite stream of pairs.
; Weight strategy takes pair and returns it's weight number.
; Pairs of the resulting stream are ordered weight ascending.
(define (pair-streams-weighted a b make-pair weight)
 (define (selector u v w)
  (define x (weight (stream-car u)))
  (define y (weight (stream-car v)))
  (define z (weight (stream-car w)))
  (if (<= x y) (if (<= x z) u w) (if (<= y z) v w))
 )

 (pair-streams-selected a b make-pair selector)
)

; Pairs infinite streams in a diagonal order. Sample for integer
; pairs: (1 1) (1 2) (2 1) (1 3) (2 2) (3 1) (1 4) (2 3) (3 2)...
; You may split it into groups by sum equality, ascending.
(define (pair-streams a b)
 (define ia (stream-map cons integers a))
 (define ib (stream-map cons integers b))

 (define (make-pair ix iy)
  (cons
   (+ (car ix) (car iy))
   (cons (cdr ix) (cdr iy))
  )
 )

 (stream-map cdr (pair-streams-weighted ia ib make-pair car))
)

; Like «pair-streams-selected», creates triples from 3 streams.
; Selector takes multiple arguments being streams of triples,
; it must select and return one of these streams.
(define (triple-streams-selected a b c make-triple selector)
 (define (find->interleave s head tail)
  (if (not (eq? s (car tail)))
   (find->interleave s (cons (car tail) head) (cdr tail))
   (cons-stream
    (stream-car s)
    (apply interleave
     (append
      (reverse head)
      (list (stream-cdr s))
      (cdr tail)
     )
    )
   )
  )
 )

 (define (interleave . streams)
  (find->interleave (apply selector streams) '() streams)
 )

 (define (triple-Xyz X y z)
  (pair-streams-selected y z
   (lambda (Y Z) (make-triple X Y Z))
   selector
  )
 )

 (define (triple-xYz x Y z)
  (pair-streams-selected x z
   (lambda (X Z) (make-triple X Y Z))
   selector
  )
 )

 (define (triple-xyZ x y Z)
  (pair-streams-selected x y
   (lambda (X Y) (make-triple X Y Z))
   selector
  )
 )

 (define (triple-XYz X Y z)
  (stream-map (lambda (Z) (make-triple X Y Z)) z)
 )

 (define (triple-XyZ X y Z)
  (stream-map (lambda (Y) (make-triple X Y Z)) y)
 )

 (define (triple-xYZ x Y Z)
  (stream-map (lambda (X) (make-triple X Y Z)) x)
 )

 (define (nest x y z)
  (cons-stream
   (make-triple (stream-car x) (stream-car y) (stream-car z))
   (interleave
    (triple-Xyz (stream-car x) (stream-cdr y) (stream-cdr z))
    (triple-xYz (stream-cdr x) (stream-car y) (stream-cdr z))
    (triple-xyZ (stream-cdr x) (stream-cdr y) (stream-car z))
    (triple-XYz (stream-car x) (stream-car y) (stream-cdr z))
    (triple-XyZ (stream-car x) (stream-cdr y) (stream-car z))
    (triple-xYZ (stream-cdr x) (stream-car y) (stream-car z))
    (nest (stream-cdr x) (stream-cdr y) (stream-cdr z))
   )
  )
 )

 (nest a b c)
)

(define (triple-streams-weighted a b c make-triple weight)
 (define (selector . streams)
  (define weights (map weight (map stream-car streams)))
  (define minw (apply min weights))

  (define (find ws s)
   (if (= minw (car ws)) (car s)
    (find (cdr ws) (cdr s))
   )
  )

  (find weights streams)
 )

 (triple-streams-selected a b c make-triple selector)
)

(define (triple-streams a b c)
 (define ia (stream-map cons integers a))
 (define ib (stream-map cons integers b))
 (define ic (stream-map cons integers c))

 (define (make-triple ix iy iz)
  (cons
   (+ (car ix) (car iy) (car iz))
   (list (cdr ix) (cdr iy) (cdr iz))
  )
 )

 (stream-map cdr (triple-streams-weighted ia ib ic make-triple car))
)
