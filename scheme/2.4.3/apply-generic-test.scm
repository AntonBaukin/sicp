(define (log . args) (for-each display args) (newline))

(include "apply-generic.scm")
(include "../2.3.3/tree-print.scm")

(define apply-generic-tree->str (make-tree->str-printer
  ApplyGenericTable apply-generic-op-key->str)
)

(define (apply-generic-log-tree)
 (log "Apply generic table tree\n"
  (apply-generic-tree->str apply-generic-table))
)

(define tagged-123 (apply-generic-tag 'number 123))
(log "tagged 123 —> " tagged-123)
(log "123 tagged ?= " (apply-generic-tagged? tagged-123))
(log "123 tagged 'number ?= " (apply-generic-tagged? tagged-123 'number))
(log "123 tagged 'abc ?= " (apply-generic-tagged? tagged-123 'abc))

(define op-key-plus (apply-generic-make-op-key '+ '(number number) +))
(define op-key-sub (apply-generic-make-op-key '- '(number number) -))
(define op-key-neg (apply-generic-make-op-key '- '(number) -))
(log "op-key-plus = " (apply-generic-op-key->str op-key-plus))
(log "op-key-sub = " (apply-generic-op-key->str op-key-sub))
(log "op-key-neg = " (apply-generic-op-key->str op-key-neg))

(apply-generic-put '+ '(number number) +)
(apply-generic-put '- '(number number) -)
(apply-generic-put '- '(number) -)
(apply-generic-log-tree)
(set! apply-generic-table '())

(apply-generic-put-all
 '+ '(number number) (lambda (a b) (+ a b))
 '+ '(string number) (lambda (s n) (string-append s (number->string n)))
 '- '(number number) (lambda (a b) (- a b))
 '- '(number) (lambda (a) (- a))
)
(apply-generic-log-tree)

(log "Get (+ n n) —> " (apply-generic-get '+ '(number number)))
(log "Get (- n) —> " (apply-generic-get '- '(number)))
(log "Get (+ s n) —> " (apply-generic-get '+ '(string number)))
(log "Get (+ s s) —> " (apply-generic-get '+ '(string string)))

(define tagged-abc (apply-generic-tag 'string "abc"))
(log "abc + 123 = " (apply-generic '+ tagged-abc tagged-123))
