; By default, debug mode is off.
(define eval-debug-mode #f)

; More than in SICP: number, boolean, string, character.
(define (self-evaluating? exp)
 (or
  (number? exp)
  (eq? #t exp)
  (eq? #f exp)
  (string? exp)
  (char? exp)
 )
)

(define (variable? exp)
 (symbol? exp)
)

(define (tagged-list? l tag)
 (and
  (list? l)
  (eq? tag (car l))
 )
)

(define (quoted? exp)
 (tagged-list? exp 'quote)
)

(define (text-of-quotation exp)
 (list-ref exp 1)
)

(define (definition? exp)
 (tagged-list? exp 'define)
)

(define (define-get-variable exp)
 (if (symbol? (cadr exp))
  (cadr exp)
  (caadr exp)
 )
)

(define (define-get-value exp)
 (if (symbol? (cadr exp))
  (caddr exp)
  (make-lambda (cdadr exp) (cddr exp))
 )
)

; See, how loose it is: it takes everything!
(define (application? exp)
 (list? exp)
)

(define (list-of-values exps env)
 (if (null? exps) '()
  (let ((value (eval-impl (car exps) env)))
   (cons value (list-of-values (cdr exps) env))
  )
 )
)

(define (operator exp)
 (car exp)
)

(define (operands exp)
 (cdr exp)
)

(define (compound-procedure? p)
 (tagged-list? p 'procedure)
)

(define (procedure-parameters p)
 (list-ref exp 1)
)

(define (procedure-body p)
 (list-ref exp 2)
)

(define (procedure-environment p)
 (list-ref exp 3)
)

