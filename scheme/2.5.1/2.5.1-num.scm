
(define (install-number-package-base scope TAG)
 (define TAG2 (list (car TAG) (car TAG)))

 (define (call-and-tag op a b)
  (num-tag-set (car TAG) (op a b))
 )

 (define (float-div a b)
  (/ (* 1.0 a) b)
 )

 (define (number->short-string n)
  (number->string (* 0.001 (round (* n 1000))))
 )

 ((apply-generic-scope-register scope)
  'num TAG (lambda (n) (num-tag-set (car TAG) n))
  'str TAG number->string
  'short-str TAG number->short-string

  'add TAG2 (curry call-and-tag +)
  'sub TAG2 (curry call-and-tag -)
  'mul TAG2 (curry call-and-tag *)
  'div TAG2 (curry call-and-tag float-div)
 )

 TAG ;<— return the tag list
)

(define (install-number-package scope)
 (install-number-package-base scope '(number))
)

(define-value-if-not 'make-number (curry make-num 'num 'number))
