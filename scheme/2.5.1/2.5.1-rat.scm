
(define (install-rational-package scope)
 (include "../2.1/2.1.1-2.1.scm")

 (define (call-and-tag op a b)
  (num-tag-set 'rational (op a b))
 )

 ((apply-generic-scope-register scope)
  'num '(rational) (curry call-and-tag rat-make)
  'str '(rational) rat-str

  'add '(rational rational) (curry call-and-tag rat-add)
  'sub '(rational rational) (curry call-and-tag rat-sub)
  'mul '(rational rational) (curry call-and-tag rat-mul)
  'div '(rational rational) (curry call-and-tag rat-div)
 )
)

(define make-rat (curry make-num 'num 'rational))
