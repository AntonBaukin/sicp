; By default, debug mode is off.
(define debug-mode? #f)

(define (debug-set on?)
 (set! debug-mode? on?)
)

(define-macro (if-debug . script)
 `(if debug-mode? (begin ,@script))
)

; Evaluator reference to use in recursive calls.
(define eval-impl void)

; Applicator reference to use in recursive calls.
(define apply-impl void)

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
; + log-stack  prints current stack;
;
; + pause  reads from console input (just press «Enter»);
;
; + on  turns debugging mode on;
;
; + off turns debugging mode off;
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
  (not (null? l))
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

(define (global-definition? exp)
 (tagged-list? exp 'global)
)

(define (define-get-variable exp)
 (if (symbol? (cadr exp))
  (cadr exp)
  (caadr exp)
 )
)

; For variable length arguments function, the arguments
; is not a list of symbols, but a generic sequence of
; pairs — CDR of the terminal pair is the name of
; vararg argument.
;
; Also see «varargs-args-combine» function.
;
(define (define-make-lambda-varargs args)
 (define (next args res)
  (if (symbol? args)
   (cons (reverse res) args)
   (next
    (cdr args)
    (if (eq? void (car args)) res
     (cons (car args) res)
    )
   )
  )
 )

 (next args '())
)

(define (define-make-lambda-args args)
 (if (list? args) args
  (define-make-lambda-varargs args)
 )
)

(define (define-make-lambda exp)
 (make-lambda (cdadr exp) (cddr exp))
)

(define (define-get-value exp)
 (if (symbol? (cadr exp))
  (caddr exp)
  (define-make-lambda exp)
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

(define DOT_SYMBOL (string->symbol "."))

(define (lambda-short-varargs? args)
 (and
  (list? args)
  (= 2 (length args))
  (eq? DOT_SYMBOL (car args))
 )
)

(define (lambda-parameters exp)
 (define args (cadr exp))

 (if (lambda-short-varargs? args)
  (set! args (cons void (cadr args)))
 )

 (define-make-lambda-args args)
)

(define (lambda-body exp)
 (cddr exp)
)

(define (if? exp)
 (tagged-list? exp 'if)
)

(define (make-if predicate consequent alternative)
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

(define (assignment? exp)
 (tagged-list? exp 'set!)
)

(define (assignment-variable exp)
 (cadr exp)
)

(define (assignment-value exp)
 (caddr exp)
)

(define (eval-assignment exp env)
 (assign-variable
  env
  (assignment-variable exp)
  (eval-impl (assignment-value exp) env)
 )
)

(define (begin? exp)
 (tagged-list? exp 'begin)
)

(define (begin-actions exp)
 (cdr exp)
)

(define (cond? exp)
 (tagged-list? exp 'cond)
)

(define (cond-clauses exp)
 (cdr exp)
)

(define (cond-actions clause)
 (cdr clause)
)

(define (cond->if exp)
 (expand-clauses (cond-clauses exp))
)

(define (cond-predicate clause)
 (car clause)
)

(define (cond-else-clause? clause)
 (eq? 'else (cond-predicate clause))
)

(define (expand-clauses clauses)
 (if (null? clauses) void
  (if (cond-else-clause? (car clauses))
   (if (null? (cdr clauses))
    (sequence->exp (cond-actions (car clauses)))
    (error ("Condition else clause is not the last" clauses))
   )
   (make-if
    (cond-predicate (car clauses))
    (sequence->exp (cond-actions (car clauses)))
    (expand-clauses (cdr clauses))
   )
  )
 )
)

(define (last-exp? seq)
 (null? (cdr seq))
)

(define (first-exp seq)
 (car seq)
)

(define (make-begin seq)
 (cons 'begin seq)
)

(define (sequence->exp seq)
 (cond
  ((null? seq) seq)
  ((last-exp? seq) (first-exp seq))
  (else (make-begin seq))
 )
)

(define (apply? exp)
 (tagged-list? exp 'apply)
)

(define (apply-call exp env)
 (eval-impl
  (append
   (list (cadr exp))
   (eval-impl (caddr exp) env)
  )
  env
 )
)
