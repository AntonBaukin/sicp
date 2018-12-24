; this file is included in tasks 2.7 - 2.14

(define (log . args) (for-each display args) (newline))

(define (interval-make a b)
 (cons (min a b) (max a b))
)

(define (interval-low i)
 (car i)
)

(define (interval-up i)
 (cdr i)
)

(define (interval-eq? i j)
 (and
  (= (interval-low i) (interval-low j))
  (= (interval-up i) (interval-up j))
 )
)

(define (interval-str i)
 (string-append
  "["
  (number->string (interval-low i))
  " .. "
  (number->string (interval-up i))
  "]"
 )
)

(define (interval-add i j)
 (interval-make
  (+ (interval-low i) (interval-low j))
  (+ (interval-up i) (interval-up j))
 )
)

(define (interval-mul i j)
 (let
  (
   (ab (* (interval-low i) (interval-low j)))
   (cd (* (interval-low i) (interval-up j)))
   (ef (* (interval-up i) (interval-low j)))
   (gh (* (interval-up i) (interval-up j)))
  )

  (interval-make
    (min ab cd ef gh)
    (max ab cd ef gh)
  )
 )
)

(define (interval-inverse i)
 (interval-make
  (/ 1.0 (interval-up i))
  (/ 1.0 (interval-low i))
 )
)

(define (interval-div i j)
 (interval-mul i (interval-inverse j))
)
