
; Converts number to string. This version rounds it up.
(define (complex-utils-number->string n)
 (number->string (* 0.001 (round (* n 1000))))
)

; Converts radians value to string appending «°» symbol.
(define (complex-utils-radians->string r)
 (string-append
  (complex-utils-number->string
   (/ (* 180.0 r) 3.14159265359)
  )
  "°"
 )
)


; Collection of number utils required for complex
; package. In 2.5.2-2.86 we will overwrite it with
; generic analogues that stay the same data type.
(define-value-if-not 'complex-utils (list
 +
 *
 square
 sqrt
 cos
 sin
 atan
 -
 /
 complex-utils-number->string
 complex-utils-radians->string
))

(define (install-complex-rect-package scope)
 (define TAG '(rectangular))

 (define ad (list-ref complex-utils 0))
 (define mu (list-ref complex-utils 1))
 (define sq (list-ref complex-utils 2))
 (define sr (list-ref complex-utils 3))
 (define co (list-ref complex-utils 4))
 (define si (list-ref complex-utils 5))
 (define at (list-ref complex-utils 6))

 (define (make x y) (cons x y))
 (define (real z) (car z))
 (define (imag z) (cdr z))

 (define (mag z)
  (sr (ad (sq (real z)) (sq (imag z))))
 )

 (define (ang z)
  (at (imag z) (real z))
 )

 (define (make-from-mag-ang r a)
  (make (mu r (co a)) (mu r (si a)))
 )

 (define (call-and-tag f)
  (lambda (a b) (apply-generic-tag (car TAG) (f a b)))
 )

 ((apply-generic-scope-register scope)
  'real-part TAG real
  'imag-part TAG imag
  'magnitude TAG mag
  'angle     TAG ang

  'make-from-real-imag TAG (call-and-tag make)
  'make-from-mag-ang   TAG (call-and-tag make-from-mag-ang)
 )

 TAG ;<— return the tag list
)

(define (install-complex-polar-package scope)
 (define TAG '(polar))

 (define ad (list-ref complex-utils 0))
 (define mu (list-ref complex-utils 1))
 (define sq (list-ref complex-utils 2))
 (define sr (list-ref complex-utils 3))
 (define co (list-ref complex-utils 4))
 (define si (list-ref complex-utils 5))
 (define at (list-ref complex-utils 6))

 (define (make r a) (cons r a))
 (define (mag z) (car z))
 (define (ang z) (cdr z))

 (define (real z) (car z)
  (mu (mag z) (co (ang z)))
 )

 (define (imag z) (car z)
  (mu (mag z) (si (ang z)))
 )

 (define (make-from-real-imag x y)
  (make (sr (+ (sq x) (sq y))) (at y x))
 )

 (define (call-and-tag f)
  (lambda (a b) (apply-generic-tag (car TAG) (f a b)))
 )

 ((apply-generic-scope-register scope)
  'real-part TAG real
  'imag-part TAG imag
  'magnitude TAG mag
  'angle     TAG ang

  'make-from-real-imag TAG (call-and-tag make-from-real-imag)
  'make-from-mag-ang   TAG (call-and-tag make)
 )

 TAG ;<— return the tag list
)

(define (install-complex-package scope)
 (define TAG  '(complex))
 (define TAG2 '(complex complex))

 (define ad (list-ref complex-utils 0))
 (define mu (list-ref complex-utils 1))
 (define su (list-ref complex-utils 7))
 (define dv (list-ref complex-utils 8))

 ; We use distinct scope to define complex-internal packages
 (define complex-scope  (apply-generic-make-default))
 (define complex-lookup (apply-generic-scope-lookup complex-scope))
 (define complex-apply  (apply-generic-scope-function complex-scope))

 ; Install private packages into that scope
 (define TAGR (install-complex-rect-package complex-scope))
 (define TAGP (install-complex-polar-package complex-scope))

 ; Directly take constructors as they do work with raw types
 (define make-from-real-imag (complex-lookup 'make-from-real-imag TAGR))
 (define make-from-mag-ang   (complex-lookup 'make-from-mag-ang TAGP))

 ; Delegates to the private scope calls
 (define (real-part z) (complex-apply 'real-part z))
 (define (imag-part z) (complex-apply 'imag-part z))
 (define (magnitude z) (complex-apply 'magnitude z))
 (define (angle     z) (complex-apply 'angle     z))

 ; Complex arithmetics in the best system
 (define (add a b)
  (make-from-real-imag
   (ad (real-part a) (real-part b))
   (ad (imag-part a) (imag-part b))
  )
 )

 (define (sub a b)
  (make-from-real-imag
   (su (real-part a) (real-part b))
   (su (imag-part a) (imag-part b))
  )
 )

 (define (mul a b)
  (make-from-mag-ang
   (mu (magnitude a) (magnitude b))
   (ad (angle a) (angle b))
  )
 )

 (define (div a b)
  (make-from-mag-ang
   (dv (magnitude a) (magnitude b))
   (su (angle a) (angle b))
  )
 )

 (define n->s (list-ref complex-utils 9))
 (define degrees-str (list-ref complex-utils 10))

 (define (complex-str z)
  (string-append
   "(x: " (n->s (real-part z))
   " y: " (n->s (imag-part z))
   " r: " (n->s (magnitude z))
   " a: " (degrees-str (angle z))
   ")"
  )
 )

 (define (call-and-tag op a b)
  (num-tag-set (car TAG) (op a b))
 )

 ((apply-generic-scope-register scope)
  'num-xy TAG (curry call-and-tag make-from-real-imag)
  'num-ra TAG (curry call-and-tag make-from-mag-ang)
  'str    TAG complex-str

  'add TAG2 (curry call-and-tag add)
  'sub TAG2 (curry call-and-tag sub)
  'mul TAG2 (curry call-and-tag mul)
  'div TAG2 (curry call-and-tag div)
 )

 ; We export these internal functions as they are
 ; required for some exercises.
 (list TAG real-part imag-part magnitude angle)
)

(define make-complex-xy (curry make-num 'num-xy 'complex))
(define make-complex-ra (curry make-num 'num-ra 'complex))
