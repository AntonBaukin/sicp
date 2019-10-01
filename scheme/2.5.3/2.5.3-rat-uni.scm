
; Replaces 2.5.1-rat.scm implementation. Uses generic
; functions to make arithmetic operations.
;
; Requires cut() generic function that takes two generic
; numbers, finds GCD and divides by it.
;
(define (install-rational-uni-package scope)
 (define TAG  '(rational))
 (define TAG2 '(rational rational))


 ; Cuts common factors of two generic numbers.
 ; Returns pair of resulting numbers.
 ;
 ; This generic function is much better than
 ; finding greater-common-divisor and then
 ; applying generic devision.
 ;
 (define cut (curry num-call 'cut))

 (define (make-rat n d)
  (cut n d) ;<— pair that we do require
 )

 (define (add-rat a b)
  (make-rat
   (add
    (mul (car a) (cdr b))
    (mul (cdr a) (car b))
   )
   (mul (cdr a) (cdr b))
  )
 )

 (define (sub-rat a b)
  (make-rat
   (sub
    (mul (car a) (cdr b))
    (mul (cdr a) (car b))
   )
   (mul (cdr a) (cdr b))
  )
 )

 (define (mul-rat a b)
  (make-rat
   (mul (car a) (car b))
   (mul (cdr a) (cdr b))
  )
 )

 (define (div-rat a b)
  (make-rat
   (mul (car a) (cdr b))
   (mul (cdr a) (car b))
  )
 )

 (define (drop-safe n)
  (let* (
    (v (if (number? n) (make-number n) n))
    (x (drop-impl v))
   )
   (if (null? x) n x)
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

 (define (needs-braces? s)
  (needs-braces-iter s 0 (string-length s))
 )

 (define (wrap-in-braces s)
  (if (needs-braces? s) (string-append "[" s "]") s)
 )

 (define (rat->str r)
  (let (
    (sn (num->str (drop-safe (car r))))
    (sd (num->str (drop-safe (cdr r))))
   )

   (string-append
    (wrap-in-braces sn)
    (if
     (or (needs-braces? sn) (needs-braces? sd))
     " / " "/"
    )
    (wrap-in-braces sd)
   )
  )
 )


 (define (call-and-tag op a b)
  (num-tag-set (car TAG) (op a b))
 )

 ((apply-generic-scope-register scope)
  'num TAG (curry call-and-tag make-rat)
  'str TAG rat->str

  'add TAG2 (curry call-and-tag add-rat)
  'sub TAG2 (curry call-and-tag sub-rat)
  'mul TAG2 (curry call-and-tag mul-rat)
  'div TAG2 (curry call-and-tag div-rat)
 )

 ; Resulting TAG and make function:
 (list (car TAG) make-rat)
)

