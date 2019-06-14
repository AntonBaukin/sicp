
; As requested, we specially treat the plain numbers.
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

; The follwoing definition is not required as we re-define
; the number package not to use the tags internally.
(define (num-tag-set tag obj)
 (if (eq? 'number tag) obj
  (apply-generic-tag tag obj)
 )
)

; Special version of the numbers package that does not wrap.
; This package will net be re-installed in arithmetics.scm.
(define (install-number-package-278 scope)
 (define TAG  '(number))
 (define TAG2 '(number number))

 (define (float-div a b)
  (/ (* 1.0 a) b)
 )

 ((apply-generic-scope-register scope)
  'num TAG (lambda (n) (num-tag-set (car TAG) n))
  'str TAG number->string

  ; We directly apply them not wrapping the result.
  'add TAG2 +
  'sub TAG2 -
  'mul TAG2 *
  'div TAG2 float-div
 )

 TAG ;<— return the tag list
)

; Early-define proper numbers maker.
(define (make-number v) v)

; Now include the package and run the tests...
(include "2.5.1-test.scm")
