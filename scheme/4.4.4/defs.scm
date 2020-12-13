;
; QEval dispathing evaluator. Define it in modules.
;
(define qeval-disp void)
;
; Define list of lists: (symbolic-name sub-evaluator).
;
(define qeval-procs '())

(define (tagged? what . tag)
 (and
  (pair? what)
  (not (null? what))
  (or
   (null? tag)
   (eq? (car tag) (car what))
  )
 )
)

(define (untag tagged)
 (cdr tagged)
)

; Frames from SICP §4.4.4.8.
(define (make-frame bindings)
 (cons 'frame bindings)
)

(define empty-frame (make-frame '()))

(define (frame? what)
 (tagged? what 'frame)
)

(define (check-frame frame)
 (if (frame? frame) frame
  (error "Not a frame" frame)
 )
)

(define (frame-bindings frame)
 (cdr (check-frame frame))
)

(define (make-binding name value)
 (cons name value)
)

(define (binding-name binding)
 (car binding)
)

(define (binding-value binding)
 (cdr binding)
)

(define (pattern? what)
 (tagged? what 'pattern)
)

(define (check-pattern pattern)
 (if (pattern? pattern) pattern
  (error "Not a pattern" pattern)
 )
)

(define (make-pattern query)
 (cons 'pattern (check-query query))
)

(define (rule? what)
 (tagged? what 'rule)
)

(define (rule-correct? rule)
 (and
  (list? rule)
  (rule? rule)
  (or
   (= 2 (length rule))
   (= 3 (length rule))
  )
  (list? (cadr rule))
  ; TODO: check rule deeply
 )
)

(define (check-rule rule)
 (if (rule-correct? rule) rule
  (error "Not a valid rule" rule)
 )
)

(define (assertion? what)
 (tagged? what 'assertion)
)

(define (check-assertion assertion)
 (if (assertion? assertion) assertion
  (error "Not an assertion" assertion)
 )
)

(define (statement? what)
 (and 
  (list? what)
  (< 1 (length what))
  (constant-symbol? (car what))
  (not (variable-symbol? (car what)))
  ; TODO: check statement deeply
 )
)

(define (check-statement statement)
 (if (statement? statement) statement
  (error "Not a valid statement" statement)
 )
)

(define (query? what)
 (and 
  (list? what)
  (< 0 (length what))
  (or
   (constant-symbol? (car what))
   (variable? (car what))
  )
  ; TODO: check query deeply
 )
)

(define (check-query query)
 (if (query? query) query
  (error "Not a valid query" query)
 )
)

(define (make-assertion statement)
 (cons 'assertion (check-statement statement))
)

(define (constant-symbol? exp)
 (symbol? exp)
)

(define (variable-symbol? exp)
 (and
  (symbol? exp)
  (equal? "?" (string-copy (symbol->string exp) 0 1))
 )
)

(define (variable? what)
 (and
  (pair? what)
  (eq? '? (car what))
  (symbol? (cdr what))
 )
)

(define (variable-name var)
 (cdr var)
)

(define (conclusion rule)
 (cadr rule)
)
