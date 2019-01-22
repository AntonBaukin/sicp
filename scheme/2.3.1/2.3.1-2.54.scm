(define (log . args) (for-each display args) (newline))

(define (equals? a b)
 (cond
  ((and (symbol? a) (symbol? b))
   (eq? a b))

  ((and (number? a) (number? b))
   (= a b))

  ((and (null? a) (null? b)) #t)

  ((and (pair? a) (pair? b))
   (and
     (equals? (car a) (car b))
     (equals? (cdr a) (cdr b))
   )
  )

  (else #f)
 )
)

(log (equals? 'a 'b) "  'a ?= 'b" )
(log (equals? 'a 'a) "  'a ?= 'a" )
(log (equals? 'a "a") "  'a ?= \"a\"" )
(log (equals? 1 1.0) "  '1 ?= 1.0" )
(log (equals? 2 1) "  '2 ?= 1" )
(log (equals? '(a b c) '(a b c)) "  '(a b c) ?= '(a b c)" )
(log (equals? '(a b c) '(a b)) "  '(a b c) ?= '(a b)" )
(log (equals? '(a . b) '(a . b)) "  '(a . b) ?= '(a . b)" )
(log (equals? '(a . b) '(a . c)) "  '(a . b) ?= '(a . c)" )
(log (equals? '(1 . 2) '(1.0 . 2.0)) "  '(1 . 2) ?= '(1.0 . 2.0)" )
(log (equals? '((a) (b (c))) '((a) (b (c)))) "  '((a) (b (c))) ?= '((a) (b (c))))" )
(log (equals? '((a) (b (c))) '((a) (b  c))) "  '((a) (b (c))) ?= '((a) (b (c))))" )
(log (equals? '((a) (b c ())) '((a) (b c))) "  '((a) (b c ())) ?= '((a) (b c)))" )
