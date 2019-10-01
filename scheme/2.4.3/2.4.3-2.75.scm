(include "curry.scm")

(define (log . args) (for-each display args) (newline))

(define (apply-generic op arg . args) (arg op args))

(define (dispatch-real-imag x y op args)
 (cond
  ((eq? op 'real-part) x)
  ((eq? op 'imag-part) y)
  ((eq? op 'angle) (atan y x))

  ((eq? op 'magnitude)
   (sqrt (+ (square x) (square y)))
  )

  ((eq? op 'string) (string-append
   "(x: " (number->string x)
   " y: " (number->string y) ")"
  ))

  ((eq? op '+) (make-from-real-imag
   (+ x (apply-generic 'real-part (car args)))
   (+ y (apply-generic 'imag-part (car args)))
  ))

  (else (error "Unknown complex real-imag op: " op))
 )
)

(define (make-from-real-imag x y)
 (curry dispatch-real-imag x y)
)

(define (dispatch-mag-ang r a op args)
 (cond
  ((eq? op 'angle) a)
  ((eq? op 'magnitude) r)
  ((eq? op 'real-part) (* r (cos a)))
  ((eq? op 'imag-part) (* r (sin a)))

  ((eq? op 'string) (string-append
   "(r: " (number->string r)
   " a: " (number->string a) ")"
  ))

  ((eq? op '+) (make-from-real-imag
   (+ (* r (cos a)) (apply-generic 'real-part (car args)))
   (+ (* r (sin a)) (apply-generic 'imag-part (car args)))
  ))

  (else (error "Unknown complex mag-ang op: " op))
 )
)

(define (make-from-mag-ang r a)
 (curry dispatch-mag-ang r a)
)

(define pi 3.14159265359)

(define (degrees-str radians)
 (string-append (number->string (/ (* 180.0 radians) pi)) "°")
)

(define xy34 (make-from-real-imag 3 4))
(define xy44 (make-from-real-imag 4 -4))
(define ra44 (make-from-mag-ang 5.65685424949238 (* pi 3 0.25)))

(log "xy34 = " (apply-generic 'string xy34))
(log "xy44 = " (apply-generic 'string xy44))
(log "|xy34| = " (apply-generic 'magnitude xy34))
(log "a(xy44) = " (degrees-str (apply-generic 'angle xy44)))
(log "xy34 + xy44 = " (apply-generic 'string (apply-generic '+ xy34 xy44)))

(log "ra44 = " (apply-generic 'string ra44))
(log "a(ra44) = " (degrees-str (apply-generic 'angle ra44)))
(log "re(ra44) = " (apply-generic 'real-part ra44))
(log "im(ra44) = " (apply-generic 'imag-part ra44))
(log "ra44 + xy44 = " (apply-generic 'string (apply-generic '+ ra44 xy44)))
