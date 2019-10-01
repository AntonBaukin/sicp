
; Creates function formatting polynomial to string.
; It is rather complex, with superscript powers, and
; reduced coefficients with scope detection.
(define (make-polynomial->str)
 (include "superscript-digits.scm")

 ; Converts coefficient to a string. It has special
 ; trearing for strings ending with period: it cuts
 ; «.» out to make the number prettier.
 (define (coeff->str n)
  (let* (
    (s (num->str n))
    (i (- (string-length s) 1))
    (x (string-ref s i))
   )
   (if (not (eq? x #\.)) s
    (substring s 0 i)
   )
  )
 )

 ; Converts order and term variable to a superscript
 ; power of this variable.
 (define (order->str v o)
  (cond
   ((= 0 o) "")
   ((= 1 o) (symbol->string v))
   (else (string-append
    (symbol->string v)
    (replace-superscript-digits (number->string o))
   ))
  )
 )

 (define (digit? c)
  (let ((s (string c)))
   (and
    (string>=? s "0")
    (string<=? s "9")
   )
  )
 )

 (define (needs-braces-iter s i l)
  (if (= i l) #f
   (let ((c (string-ref s i)))
    (if (or (eq? c #\.) (eq? c #\-) (digit? c))
     (needs-braces-iter s (+ i 1) l)
     #t
    )
   )
  )
 )

 (define (needs-braces? cs)
  (needs-braces-iter cs 0 (string-length cs))
 )

 (define (one? cs)
  (or (equal? cs "1") (equal? cs "1.") (equal? cs "1.0"))
 )

 ; Simplified version of plain one number coefficient test.
 (define (omit-one o cs)
  (if (= 0 o) cs ;<— not omit one coeff for the last term (0-power)
   (if (one? cs)  "" cs)
  )
 )

 (define (coeff-junction plus minus o cs)
  (let ((x (string-ref cs 0)))
   (string-append
    (if (eq? x #\-) minus plus)
    (if (eq? x #\-)
     (omit-one o (substring cs 1 (string-length cs)))
     (omit-one o cs)
    )
   )
  )
 )

 ; Plus-junction for the terms. For coefficients starting
 ; with «-» prints minus-junction omitting the coeff sign.
 (define plus-coeff (curry coeff-junction " + " " - "))

 ; Extension of omit-one() for the leading term.
 (define omit-one-first (curry coeff-junction "" "-"))

 (define (term->str v o c s)
  (let ((cs (coeff->str c)))
   (string-append
    (cond
     ; Coeff is not a plain number format?
     ((needs-braces? cs)
      (string-append
       (if (= 0 (string-length s)) "" " + ")
       "(" cs ")"
      )
     )

     ; It's a first coeff?
     ((= 0 (string-length s)) (omit-one-first o cs))

     ; Add it via a junction.
     (else (plus-coeff o cs))
    )

    (order->str v o)
   )
  )
 )

 (define (terms->str-iter str var terms)
  (if (null? terms) str
   (terms->str-iter
    (string-append str
     (term->str var (caar terms) (cdar terms) str)
    )
    var
    (cdr terms)
   )
  )
 )


 (curry terms->str-iter "") ;<— resulting function
)
