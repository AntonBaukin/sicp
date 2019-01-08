(include "2.1.4-2.x.scm")

(define (interval-mul-smart-make)

 ; There are 3 base variants for each (a .. b):
 ;   p — 0 < a < b
 ;   n - a < b < 0
 ;   m - a ≤ 0 ≤ b
 ;
 ; For two intervals these gives 9 combinations:
 ;   pp pn pm  ;  np nn nm  ;  mp mn mm
 ;
 ; For each combination we define a mul-function:

 (define combinations (list
  "pp" (lambda (a b c d)
   (list (* a c ) (* b d))
  )

  "nn" (lambda (a b c d)
   (list (* b d ) (* a c))
  )

  "pn" (lambda (a b c d)
   (list (* b c ) (* a d))
  )

  "np" (lambda (a b c d)
   (list (* a d ) (* b c))
  )

  "pm" (lambda (a b c d)
   (list (* b c ) (* b d))
  )

  "mp" (lambda (a b c d)
   (list (* a d ) (* b d))
  )

  "nm" (lambda (a b c d)
   (list (* a d ) (* a c))
  )

  "mn" (lambda (a b c d)
   (list (* b c) (* a c))
  )

  "mm" (lambda (a b c d)
   (list (min (* a d) (* b c)) (max (* a c) (* b d)))
  )
 ))

 (define (find-combination xx)
  (define (next l)
   (if (equal? xx (car l))
    (cadr l)
    (next (cddr l))
   )
  )

  (next combinations)
 )

 (define (npm a b)
  (cond ((< b 0) "n")((> a 0) "p") (else "m"))
 )

 (define (name-combination a b c d)
  (string-append (npm a b) (npm c d))
 )

 (define (select-combination a b c d)
  (find-combination (name-combination a b c d))
 )

 (lambda (i j)
  (let
   (
     (a (interval-low i))
     (b (interval-up i))
     (c (interval-low j))
     (d (interval-up j))
   )

   (apply
    interval-make
    ((select-combination a b c d) a b c d)
   )
  )
 )
)

(define interval-mul-smart (interval-mul-smart-make))

(define test-cases (list
 (list  2  3  4  5)   ; pp
 (list -2 -3 -4 -5)   ; nn
 (list  2  3 -4 -5)   ; pn
 (list -4 -5  2  3)   ; np
 (list  2  3 -4  5)   ; pm
 (list -2  3  4  5)   ; mp
 (list -2 -3 -4  5)   ; nm
 (list -2  3 -4 -5)   ; mn
 (list -2  3 -4  5)   ; mm
))

(define (run-test a b c d)
 (define (num-str x)
  (if (< x 0)
   (string-append " " (number->string x))
   (string-append " +" (number->string x))
  )
 )

 (let
  (
   (i (interval-mul-smart (interval-make a b) (interval-make c d)))
   (j (interval-mul (interval-make a b) (interval-make c d)))
  )

  (log "(" (num-str a) (num-str b) (num-str c) (num-str d) " ) "
   (interval-str i) " passes " (if (interval-eq? i j) "ok" "failed!")
  )
 )
)

(for-each (lambda (case) (apply run-test case)) test-cases)
