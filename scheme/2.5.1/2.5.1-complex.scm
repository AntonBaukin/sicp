
(define (install-complex-package)


 (define (call-and-tag op a b)
  (num-tag-set 'complex (op a b))
 )

 (apply-generic-put-all
  'num '(complex) (curry call-and-tag rat-make)
  'str '(complex) rat-str

  'add '(complex complex) (curry call-and-tag add)
  'sub '(complex complex) (curry call-and-tag sub)
  'mul '(complex complex) (curry call-and-tag mul)
  'div '(complex complex) (curry call-and-tag div)
 )
)

(define make-complex (curry make-num 'complex))
