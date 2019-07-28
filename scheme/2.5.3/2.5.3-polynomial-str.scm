
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

 ; Simplified version of plain one number coefficient test.
 (define (omit-one o cs)
  (if (= 0 o) cs ;<— not omit one coeff for the last term (0-power)
   (if (or (equal? cs "1") (equal? cs "1.") (equal? cs "1.0")) "" cs)
  )
 )

 ; Plus-junction for the terms. For coefficients starting
 ; with «-» prints minus-junction omitting the coeff sign.
 (define (plus-coeff o cs)
  (let ((x (string-ref cs 0)))
   (if (eq? x #\-)
    (string-append " - " (omit-one o (substring cs 1 (string-length cs))))
    (string-append " + " (omit-one o cs))
   )
  )
 )

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
     ((= 0 (string-length s)) (omit-one o cs))

     ; Add it via a junction.
     (else (plus-coeff o cs))
    )

    (order->str v o)
   )
  )
 )

 (define (poly->str-iter var terms str)
  (if (null? terms) str
   (poly->str-iter var (cdr terms)
    (string-append str
     (term->str var (caar terms) (cdar terms) str)
    )
   )
  )
 )

 (define (poly->str poly)
  (poly->str-iter
   (car poly)
   (apply-generic-unwrap (cdr poly))
   ""
  )
 )

 poly->str ;<— resulting function
)
