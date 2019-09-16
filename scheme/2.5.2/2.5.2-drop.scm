
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
; Strategy make-number() is to create a number
; after extracting a part of complex number.
; Altered in task 2.86 where parts of a complex
; are general, not plain numbers.
;
; Predicate zero? tests «almost» zero values for
; plain numbers and may be extended for generic
; ones (see task 2.86).
;
; For «maxd» parameter see make-farey-drop-rat().
; This value is related to zero? threshold.
;
(define (install-drop-package scope maxd make-number make-integer zero?)
 (define real-part (list-ref complex-package 1))
 (define imag-part (list-ref complex-package 2))

 ; Almost zero for the real numbers only.
 (define (close? a b) (zero? (- a b)))

 ; Reduces real-only complex to real number.
 (define (drop-complex c)
  (if (not (zero? (imag-part c))) '()
   (make-number (real-part c))
  )
 )

 (include "2.5.2-farey-rat.scm")
 (define farey-drop-rat (make-farey-drop-rat maxd))

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
  ;(log "drop " n)
  (if (not (apply-generic-tagged? n)) '()
   (let ((d (get-drop (apply-generic-tag-get n))))
    (if (procedure? d) (d (apply-generic-unwrap n)) '())
   )
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
