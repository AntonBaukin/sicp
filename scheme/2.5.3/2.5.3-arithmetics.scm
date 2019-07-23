
; Arithmetics required for polynomial tasks.
; Installs all related packages and makes all
; pre- and re-definitions.

; First, we treat plain numbers as tagged.
; But we do not rewrite the numbers package
; to force back conversion as in 2.5.1-2.78.
;
(define (num-tag-get tagged-obj)
 (if (number? tagged-obj) 'number
  (apply-generic-tag-get tagged-obj)
 )
)

(define (num-unwrap tagged-obj)
 (if (number? tagged-obj) tagged-obj
  (apply-generic-unwrap tagged-obj)
 )
)


; Basic arithmetics package.
(include "../2.5.2/2.5.2-arithmetics.scm")


; The following extensions are to work with mixed
; types by raising one by the numbers tower.
(include "../2.5.2/2.5.2-try-coerce.scm")
(include "../2.5.2/2.5.2-tower.scm")
(include "../2.5.2/2.5.2-raise.scm")
(include "../2.5.2/2.5.2-try-raise-up.scm")
(include "../2.5.2/2.5.2-zero.scm")

; See task 2.5.2-2.86 for the details, but here
; we install zero? in global numbers scope.
(define (make-drop-params)
 (define maxd 10000)
 (define mine (/ 0.05 maxd))

 (define zero? (install-zero-package numbers-scope mine))

 ; Wraps plain numbers in general ones.
 (define (general-make-number n)
  (if (number? n) (make-number n) n)
 )

 (list numbers-scope maxd general-make-number zero?)
)

; We create parameters for drop function and take
; general zero? predicate from them.
(define drop-params (make-drop-params))
(define zero? (list-ref drop-params 3))

; We install drop package in an ordinary way,
; but allow to toggle it on for test purposes.
(include "../2.5.2/2.5.2-drop.scm")

; Install drop package and get that function.
(define drop-impl (apply install-drop-package drop-params))

; By default drop() is off.
(define drop (lambda (n) '()))

; Turns drop on in the desired point of the tests.
(define (toggle-drop-on!)
 (set! drop drop-impl)
)

; Installs drop() to be applied for each number-call.
; See task 2.5.2-2.86 for this generic extension point.
(define (num-call-result result)
 (let ((x (drop result)))
  (if (null? x) result x)
 )
)
