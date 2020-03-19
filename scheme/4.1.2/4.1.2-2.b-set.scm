(include "../4.1.1/eval-impl-set.scm")

; Special form «call» from Hugo...
(set! application? (lambda (exp) (tagged-list? exp 'call)))

(set! operator (lambda (exp) (cadr exp)))

(set! operands (lambda (exp) (cddr exp)))
