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

 ; Reduces real-only complex to real number.
 (define (drop-complex c)
  (if (not (zero? (imag-part c))) '()
   (make-number (real-part c))
  )
 )

 (include "2.5.2-farey-rat.scm")
 (define farey-drop-rat (make-farey-drop-rat MAXD))

 ; Reduces real number to rational.
 (define (drop-number n)
  (let ((x (farey-drop-rat n)))
   (if (pair? x)
    (make-rat (car x) (cdr x))
    (make-integer x)
   )
  )
 )

 ; Reduces rational number to integer.
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
