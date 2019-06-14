(include "defined.scm")
(include "apply-generic.scm")
(include "../2.4.3/curry.scm")

; Returns symbol tagging object wrapped in a tagged pair.
; In the following exercises we will overwrite the default
; implementations of tag get and object unwrap to allow
; extended tagging based on native Lisp types.
(define (num-tag-get tagged-obj)
; (if (procedure? num-tag-get-custom)
;  (num-tag-get-custom tagged-obj)
  (apply-generic-tag-get tagged-obj)
; )
)

(define (num-unwrap tagged-obj)
; (if (procedure? num-tag-get-custom)
;  (num-unwrap-custom tagged-obj)
  (apply-generic-unwrap tagged-obj)
; )
)

; The following tagging function is not needed for apply
; generic module, but for our number packages.
(define (num-tag-set tag obj)
; (if (procedure? num-tag-set-custom)
;  (num-tag-set-custom tag obj)
  (apply-generic-tag tag obj)
; )
)

; Global apply-generic scope for the numbers [only].
(define numbers-scope
 (apply-generic-make num-tag-get num-unwrap)
)


; Creates a number with the given variant symbol:
; use 'num when only one variant exists.
(define (make-num variant type . args)
 (define lookup (apply-generic-scope-lookup numbers-scope))
 (apply (lookup variant (list type)) args)
)

; Generic call for an operation.
(define (num-call op . args)
 (define apply-generic (apply-generic-scope-function numbers-scope))
 (apply apply-generic (cons op args))
)

; Generic arithmetics functions.
(define add (curry num-call 'add))
(define sub (curry num-call 'sub))
(define mul (curry num-call 'mul))
(define div (curry num-call 'mul))
(define num->str (curry num-call 'str))

; Gives operation character, for the tests.
(define (num-op->str op)
 (cond
  ((eq? op 'add) "+")
  ((eq? op  add) "+")
  ((eq? op 'sub) "-")
  ((eq? op  sub) "-")
  ((eq? op 'mul) "*")
  ((eq? op  mul) "*")
  ((eq? op 'div) "/")
  ((eq? op  div) "/")
  (else "?")
 )
)

(define (install-arithmetic-package package-symbol installer)
 (define-if-not package-symbol
  (lambda () (installer numbers-scope))
 )
)


; Include arithmetics modules...
(include "2.5.1-num.scm")
(install-arithmetic-package 'number-package install-number-package)

(include "2.5.1-rat.scm")
(install-arithmetic-package 'rational-package install-rational-package)

(include "2.5.1-complex.scm")
(install-arithmetic-package 'complex-package install-complex-package)
