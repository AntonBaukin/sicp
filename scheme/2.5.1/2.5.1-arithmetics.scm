(include "defined.scm")
(include "apply-generic.scm")
(include "../2.4.3/curry.scm")

; Returns symbol tagging object wrapped in a tagged pair.
; In the following exercises we will overwrite the default
; implementations of tag get and object unwrap to allow
; extended tagging based on native Lisp types.
(define-value-if-not 'num-tag-get apply-generic-tag-get)

; Returns number wrapped.
(define-value-if-not 'num-unwrap apply-generic-unwrap)

; The following tagging function is not needed for apply
; generic module, but for our number packages.
(define-value-if-not 'num-tag-set apply-generic-tag)

; Fallback lookup procedure for the numbers-specific
; apply generic. Used in §2.5.2.
(define-value-if-not 'num-call-fallback
 apply-generic-fallback-error
)

; Global apply-generic scope for the numbers [only].
(define numbers-scope
 (apply-generic-make num-tag-get num-unwrap num-call-fallback)
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
(define-value-if-not 'number-package-init install-number-package)
(install-arithmetic-package 'number-package number-package-init)

(include "2.5.1-rat.scm")
(define-value-if-not 'rational-package-init install-rational-package)
(install-arithmetic-package 'rational-package rational-package-init)

(include "2.5.1-complex.scm")
(define-value-if-not 'complex-package-init install-complex-package)
(install-arithmetic-package 'complex-package complex-package-init)
