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

; Frames from SICP §4.4.4.8 with support for task 79 (frames nesting).
(define (make-frame bindings)
 ; ('frame . (bindings . parent))
 (cons 'frame (cons bindings '()))
)

(define (frame-bindings frame)
 (cadr (check-frame frame))
)

(define (frame-parent frame)
 (cddr (check-frame frame))
)

(define (extend-frame frame bindings)
 (cons 'frame (cons bindings (frame-parent frame)))
)

(define (make-sub-frame frame bindings)
 (cons 'frame (cons bindings frame))
)

(define (derive-frame frame)
 (make-sub-frame frame '())
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

(define (make-binding name value)
 (cons name value)
)

(define (binding-name binding)
 (car binding)
)

(define (binding-value binding)
 (cdr binding)
)

(define (binding-value-unlist binding)
 (define v (binding-value binding))
 (if (and (list? v) (= 1 (length v))) (car v) v)
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

(define (rule-conclusion rule)
 (cadr rule)
)

(define (rule-body rule)
 (if (null? (cddr rule))
  '(always-true)
  (caddr rule)
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
  (complex-query-name? (car what))
  ; TODO: check query deeply
 )
)

; In §4.4.1 is said that query name may be a symbol
; or a variable. But in task 4.69 we see, that name
; may be any expression containing symbols or vars.
(define (complex-query-name? name)
 (or
  (constant-symbol? name)
  (variable? name)
  (and
   (pair? name)
   (complex-query-name? (car name))
   (or
    (null? (cdr name))
    (complex-query-name? (cdr name))
   )
  )
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
