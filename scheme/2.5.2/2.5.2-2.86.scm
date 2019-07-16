(include "../2.5.1/defined.scm")

; It SICP for task 2.5.2-2.86 it's said that you have
; to add only two generic functions: cosine and sine.
; Well, this is not fully true for 2.5.1-complex.scm!
;
; Here we have to use lambdas as the referred functions
; are not yet defined.
;
(define-value-if-not 'complex-utils (list
 ; Use generic add().
 (lambda (a b) (add a b))

 ; Use generic mul().
 (lambda (a b) (mul a b))

 ; Just (x * x)...
 (lambda (x) (mul x x))

 ; New generic function sqrt().
 (lambda (x) (num-call 'sqrt x))

 ; New generic function cos().
 (lambda (x) (num-call 'cos x))

 ; New generic function sin().
 (lambda (x) (num-call 'sin x))

 ; New generic function atan().
 (lambda (x y) (num-call 'atan x y))

 ; Use generic sub().
 (lambda (a b) (sub a b))

 ; Use generic div().
 (lambda (a b) (div a b))

 ; Converts number to string. Use generic version.
 (lambda (x) (num->short-str x))

 ; Converts radians value to string appending «°» symbol.
 ; Here we have to define a new generic function as we
 ; don't know how to define PI and 180.0 in general...
 (lambda (x) (num-call 'radians->str x))
))


; Only now we can include arithmetics...
(include "2.5.2-arithmetics.scm")

(define (log . args) (for-each display args) (newline))


(define (register-number-ops number-tag)
 (define TAG  (list number-tag))
 (define TAG2 (list number-tag number-tag))

 ; Note: regardless of the number type, we always
 ; return real numbers, not target types as results
 ; may not be represented as integers.
 (define (num-call op)
  (lambda (x) (num-tag-set 'number (op x)))
 )

 (define (num-call2 op)
  (lambda (x y) (num-tag-set 'number (op x y)))
 )

 ((apply-generic-scope-register numbers-scope)
  'sqrt TAG  (num-call  sqrt)
  'cos  TAG  (num-call  cos)
  'sin  TAG  (num-call  sin)
  'atan TAG2 (num-call2 atan)

  'radians->str TAG complex-utils-radians->string
 )
)

(register-number-ops 'number)
(register-number-ops 'integer)

;(log "sqrt 4 = " (num-call 'sqrt (make-number 4)))
;(log "radians π/2 = " (num-call 'radians->str (make-number 1.57079632679)))


; As we have no simple means to implement real functions on
; rational numbers, we have to translate rational to real,
; then invoke operation, then approximate back to a real.
(define (register-rational-ops)
 (include "2.5.2-farey-rat.scm")

 (define MAXD 10000)
 (define farey-drop-rat (make-farey-drop-rat MAXD))
 (define farey-drop-rat3 (make-farey-drop-rat 100))

 ; Invokes operation over real number returning
 ; approximated rational.
 (define (rational-approx op args)
  (let* ((r (farey-drop-rat (apply op args))))
   (if (pair? r)
    (make-rat (car r) (cdr r))
    (make-rat r 1)
   )
  )
 )

 (define (r->n r) (/ (* 1.0 (car r)) (cdr r)))

 ; Invokes real-number-operation over converted rational
 ; number pair and returns wrapped rational approximation.
 (define (rat-call op . rs)
  (rational-approx op (map r->n rs))
 )

 (define TAG  '(rational))
 (define TAG2 '(rational rational))

 ((apply-generic-scope-register numbers-scope)
  'sqrt TAG  (curry rat-call sqrt)
  'cos  TAG  (curry rat-call cos)
  'sin  TAG  (curry rat-call sin)
  'atan TAG2 (curry rat-call atan)

  ; Nothing special, just else variant.
  'radians->str TAG (lambda (radians)
   (let* (
     (n (/ (* 1.0 (car radians)) (cdr radians)))
     (d (/ (* 180.0 n) 3.14159265359))
     (r (farey-drop-rat3 d))
    )

    (if (pair? r)
     (string-append
      (number->string (car r))
      "/"
      (number->string (cdr r))
      "°"
     )

     (string-append
      (number->string r)
      "°"
     )
    )
   )
  )
 )
)

(register-rational-ops)

;(log "sqrt 1/25 = " (num-call 'sqrt (make-rat 1 25)))
;(log "sin π/6 = " (num-call 'sin (make-rat 87308 166746)))
;(log "π —> °string = " (num-call 'radians->str (make-rat 87308 27791)))


; Create complex numbers with real numbers:
(define nxy_11 (make-complex-xy
 (make-number 1) (make-number 0.5)
))

(define nxy_21 (make-complex-xy
 (make-number -2) (make-number -0.5)
))

(log "nxy_11 = " (num->str nxy_11))
(log "nxy_21 = " (num->str nxy_21))
(newline)


; Create complex numbers with rational numbers:
(define rxy_3212 (make-complex-xy
 (make-rat 3 2) (make-rat 1 2)
))

(define rxy_1212 (make-complex-xy
 (make-rat 1 2) (make-rat -1 2)
))

(log "rxy_3212 = " (num->str rxy_3212))
(log "rxy_1212 = " (num->str rxy_1212))
(newline)


; For now, we are not able to work with mixed complex types
; as we have no functions for cross-type arithmetics.
(log "nxy_11 + nxy_21 = " (num->str (add nxy_11 nxy_21)))
(log "rxy_3212 + rxy_1212 = " (num->str (add rxy_3212 rxy_1212)))


; We have to add raise-staff and more to make it possible...
(include "2.5.2-try-coerce.scm")
(include "2.5.2-tower.scm")
(include "2.5.2-raise.scm")
(include "2.5.2-try-raise-up.scm")
(include "2.5.2-zero.scm")

; And now we are able to make cross-operations!
; Still, the result is up-raised, but we may drop it...
(log "nxy_11 + rxy_1212 = " (num->str (add nxy_11 rxy_1212)))


; See task 2.85 for drop() implementation.
(include "2.5.2-drop.scm")

; Extended version of drop() that can compare each
; generic number with zero.
(define (make-drop-params)
 (define maxd 10000)
 (define mine (/ 0.05 maxd))

 ; Special generic scope for zero? predicate.
 (define zero-scope (apply-generic-make-default))
 (define zero? (install-zero-package zero-scope mine))

 ; Wraps plain numbers in general ones.
 (define (general-make-number n)
  (if (number? n) (make-number n) n)
 )
 
 (list numbers-scope maxd general-make-number zero?)
)

(define drop-impl (apply install-drop-package (make-drop-params)))


; And now we safely drop each number call...
(define (num-call-result result)
 (let ((x (drop-impl result)))
  (if (null? x) result x)
 )
)


(log "\nDropped version the last sum:")
(log "nxy_11 + rxy_1212 = " (num->str (add nxy_11 rxy_1212)))
