
; Replaces each ordinary 0..9 digit with Unicode
; superscripts ⁰¹²³⁴⁵⁶⁷⁸⁹ leaving else characters.
;
; Note: your Scheme may not support Unicode, and
; treat UTF-8 characters in a plain, but it's still
; able to concatenate them.
;
(define (replace-superscript-digits str)
 (define (next s r)
  (if (null? s) r
   (let* (
     (c (car s))
     (d (cond
      ((eq? c #\0) "⁰")
      ((eq? c #\1) "¹")
      ((eq? c #\2) "²")
      ((eq? c #\3) "³")
      ((eq? c #\4) "⁴")
      ((eq? c #\5) "⁵")
      ((eq? c #\6) "⁶")
      ((eq? c #\7) "⁷")
      ((eq? c #\8) "⁸")
      ((eq? c #\9) "⁹")
      (else (string c))
     ))
    )
    (next (cdr s) (cons d r))
   )
  )
 )

 (apply string-append
  (reverse (next (string->list str) '()))
 )
)

;(display (replace-superscript-digits "x0123456789 + abc"))
