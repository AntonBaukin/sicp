
(define (install-rational-package scope)
 (include "../2.1/2.1.1-2.1.scm")

 (define TAG  '(rational))
 (define TAG2 '(rational rational))

 (define (call-and-tag op a b)
  (num-tag-set (car TAG) (op a b))
 )

 ((apply-generic-scope-register scope)
  'num TAG (curry call-and-tag rat-make)
  'str TAG rat-str
  'numerator TAG rat-num
  'denominator TAG rat-den

  'add TAG2 (curry call-and-tag rat-add)
  'sub TAG2 (curry call-and-tag rat-sub)
  'mul TAG2 (curry call-and-tag rat-mul)
  'div TAG2 (curry call-and-tag rat-div)
 )

 TAG ;<— return the tag list
)

(define make-rat (curry make-num 'num 'rational))
