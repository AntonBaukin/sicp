; By default, debug mode is off.
(define debug-mode? #f)

(define (debug-set on?)
 (set! debug-mode? on?)
)

(define-macro (if-debug . script)
 `(if debug-mode? (begin ,@script))
)

; Evaluator reference to use in recursive calls.
(define eval-impl eval-basic)

; Applicator reference to use in recursive calls.
(define apply-impl apply-basic)

; Entry point of debug commands evaluation.
; Here is null-implementation, real one is in
; «eval-impl-debug.scm», or else script you use.
;
; Command is a symbol followed by the arguments.
; Well known commands are:
;
; + log  prints message of the given arguments;
;
; + log-env  prints environments stack except the global,
;   takes optional message arguments to log before the output;
;
; + log-stack  prints current stack.
;
(define debug-impl void)

; More than in SICP: number, boolean, string, character, or void
(define (self-evaluating? exp)
 (or
  (number? exp)
  (eq? #t exp)
  (eq? #f exp)
  (eq? void exp)
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

(define (make-procedure parameters body env)
 (list 'procedure parameters body env)
)

(define (procedure-parameters p)
 (list-ref p 1)
)

(define (procedure-body p)
 (list-ref p 2)
)

(define (procedure-environment p)
 (list-ref p 3)
)

(define (debug-command? exp)
 (tagged-list? exp 'debug)
)

(define (debug-call env exp)
 (apply debug-impl (cons env (cdr exp)))
)

(define (lambda? exp)
 (tagged-list? exp 'lambda)
)

(define (make-lambda parameters body)
 (cons 'lambda (cons parameters body))
)

(define (lambda-parameters exp)
 (cadr exp)
)

(define (lambda-body exp)
 (cddr exp)
)

(define (if? exp)
 (tagged-list? exp 'if)
)

(define (make-if prediciate consequent alternative)
 (list 'if predicate consequent alternative)
)

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
 (if (null? (cdddr exp)) void (cadddr exp))
)

(define (eval-if exp env)
 (if (eval-impl (if-predicate exp) env)
  (eval-impl (if-consequent exp) env)
  (eval-impl (if-alternative exp) env)
 )
)