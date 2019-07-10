(include "2.5.2-tower.scm")

; The implementation following resolves 2.85 task
; in clear and straight way. The approach asked
; was to «project» the number down loosing some
; info and to raise it up and compare. This
; requires two additional packages: general
; raise (task 2.84) and equ? (task 2.79).
;
; But «project» implementation knows the details
; and may tell whether exact drop is possible.
; We create drop-*() for each type thus making
; it general, and each returns '() instead of
; loosing number accuracy.
;
(define (install-drop-package scope)
 (define MAXD 10000)
 (define MINE (/ 0.05 MAXD))

 (define real-part (list-ref complex-package 1))
 (define imag-part (list-ref complex-package 2))

 ; Almost zero for the real numbers.
 (define (zero? n) (< (abs n) MINE))
 (define (close? a b) (zero? (- a b)))

 ; For the given positive fractional decimal in (0, 1)
 ; interval finds the best rational number approximating
 ; it using Farey sequence algorithm, where maxd — is the
 ; maximum denominator size. Returns (n, d) pair.
 (define (make-farey-rat maxd)
  (define (close? v r)
   (< (abs (- v r)) (/ 0.05 maxd))
  )

  (define (end? b d)
   (or (> b maxd) (> d maxd))
  )

  (define (result-end a b c d)
   (if (> b maxd) (cons c d) (cons a b))
  )

  (define (result-close a b c d)
   (cond
    ((<= (+ b d) maxd)
     (cons (+ a c) (+ b d))
    )
    ((> d b) (cons (c d)))
    (else (cons (a b)))
   )
  )

  (define (median a b c d)
   (/ (* 1.0 (+ a c)) (+ b d))
  )

  (define (next r a b c d)
   (if (end? b d) (result-end a b c d)
    (let ((m (median a b c d)))
     ; (log "a = " a " b = " b " c = " c " d = " d " m = " m)
     (cond
      ((close? m r) (result-close a b c d))
      ((> r m) (next r (+ a c) (+ b d) c d))
      (else (next r a b (+ a c) (+ b d)))
     )
    )
   )
  )

  (lambda (r) (next r 0 1 1 1))
 )

 (define farey-rat (make-farey-rat MAXD))

 ; Reduces real-only complex to real number.
 (define (drop-complex c)
  (if (not (zero? (imag-part c))) '()
   (make-number (real-part c))
  )
 )

 ; Reduces real number to rational.
 (define (drop-number n)
  (let* (
    (s (if (< n 0) -1 1))    ;<— sign
    (v (abs n))              ;<— absolute value
    (i (exact (truncate v))) ;<— integer part
    (r (- v i))              ;<— fractional part
   )
   (if (zero? r)
    (make-integer (* s i))
    ; Now approximate with rational fraction:
    (let* (
      (nd (farey-rat r))
      (r2 (/ (* 1.0 (car nd)) (cdr nd)))
     )
     
     (if (not (close? r r2)) '()
      (make-rat
       (* s (+ (* i (cdr nd))(car nd)))
       (cdr nd)
      )
     )
    )
   )
  )
 )

 ; Reduces real number to rational.
 (define (drop-rational r)
  (if (= 1 (cdr r)) (make-integer (car r)) '())
 )

 (define (get-drop type)
  ((apply-generic-scope-lookup numbers-scope)
   'drop (list type)
  )
 )

 ; Applies drop if it exists, or returns '().
 (define (drop-safe n)
  (let ((d (get-drop (apply-generic-tag-get n))))
   (if (procedure? d) (d (apply-generic-unwrap n)) '())
  )
 )

 ; Drop iterative procedure.
 (define (drop-iter res n)
  (let ((x (drop-safe n)))
   (if (null? x) res
    (drop-iter x x)
   )
  )
 )

 ; Resulting drop function.
 (define (drop n)
  ; The main feature here is that if the first try
  ; fails, we return '(); else we return the lowest
  ; available dropped value.
  (drop-iter '() n)
 )

 ; Register drop functions:
 ((apply-generic-scope-register numbers-scope)
  'drop '(complex)  drop-complex
  'drop '(number)   drop-number
  'drop '(rational) drop-rational
 )

 drop ;<— resulting drop function
)
