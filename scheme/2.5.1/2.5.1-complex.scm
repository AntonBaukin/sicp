
(define (install-complex-rect-package scope)
 (define TAG '(rectangular))

 (define (make x y) (cons x y))
 (define (real z) (car z))
 (define (imag z) (cdr z))

 (define (mag z)
  (sqrt (+ (square (real z)) (square (imag z))))
 )

 (define (ang z)
  (atan (imag z) (real z))
 )

 (define (make-from-mag-ang r a)
  (make (* r (cos a)) (* r (sin a)))
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

 (define (make r a) (cons r a))
 (define (mag z) (car z))
 (define (ang z) (cdr z))

 (define (real z) (car z)
  (* (mag z) (cos (ang z)))
 )

 (define (imag z) (car z)
  (* (mag z) (sin (ang z)))
 )

 (define (make-from-real-imag x y)
  (make (sqrt (+ (square x) (square y))) (atan y x))
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
   (+ (real-part a) (real-part b))
   (+ (imag-part a) (imag-part b))
  )
 )

 (define (sub a b)
  (make-from-real-imag
   (- (real-part a) (real-part b))
   (- (imag-part a) (imag-part b))
  )
 )

 (define (mul a b)
  (make-from-mag-ang
   (* (magnitude a) (magnitude b))
   (+ (angle a) (angle b))
  )
 )

 (define (div a b)
  (make-from-mag-ang
   (/ (magnitude a) (magnitude b))
   (- (angle a) (angle b))
  )
 )

 (define (n->s n) (number->string (* 0.001 (round (* n 1000)))))

 (define pi 3.14159265359)

 (define (degrees-str radians)
  (string-append (n->s (/ (* 180.0 radians) pi)) "°")
 )

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
)

(define make-complex-xy (curry make-num 'num-xy 'complex))
(define make-complex-ra (curry make-num 'num-ra 'complex))
