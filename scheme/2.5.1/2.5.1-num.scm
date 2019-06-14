
(define (install-number-package scope)
 (define TAG  '(number))
 (define TAG2 '(number number))

 (define (call-and-tag op a b)
  (num-tag-set (car TAG) (op a b))
 )

 (define (float-div a b)
  (/ (* 1.0 a) b)
 )

 ((apply-generic-scope-register scope)
  'num TAG (lambda (n) (num-tag-set (car TAG) n))
  'str TAG number->string

  'add TAG2 (curry call-and-tag +)
  'sub TAG2 (curry call-and-tag -)
  'mul TAG2 (curry call-and-tag *)
  'div TAG2 (curry call-and-tag float-div)
 )

 TAG ;<— return the tag list
)

(define-value-if-not 'make-number (curry make-num 'num 'number))
