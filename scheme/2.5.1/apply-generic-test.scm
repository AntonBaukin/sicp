(include "apply-generic.scm")
(include "apply-generic-search.scm")
(include "../2.3.3/tree-print.scm")

(define (log . args) (for-each display args) (newline))

(define apply-generic-tree->str
 (make-tree->str-printer
  ApplyGenericScope
  apply-generic-op-key->str
 )
)

(define (apply-generic-log-tree)
 (log "\nGlobal scope tree\n"
  (apply-generic-tree->str
   ((apply-generic-scope-internal-get-tree apply-generic-global))
  )
 )
)

(define tagged-123 (apply-generic-tag 'number 123))
(log "tagged 123 —> " tagged-123)
(log "123 tagged 'number ?= " (apply-generic-tagged? 'number tagged-123))
(log "123 tagged 'abc ?= " (apply-generic-tagged? 'abc tagged-123))

(define op-key-plus (apply-generic-make-op-key '+ '(number number) +))
(define op-key-sub (apply-generic-make-op-key '- '(number number) -))
(define op-key-neg (apply-generic-make-op-key '- '(number) -))
(log "op-key-plus = " (apply-generic-op-key->str op-key-plus))
(log "op-key-sub = " (apply-generic-op-key->str op-key-sub))
(log "op-key-neg = " (apply-generic-op-key->str op-key-neg))

(apply-generic-register
 '+ '(number number) +
 '- '(number number) -
 '- '(number) -
)

(apply-generic-log-tree)
(apply-generic-global-reset)

(apply-generic-register
 '+ '(number number) (lambda (a b) (+ a b))
 '+ '(string number) (lambda (s n) (string-append s (number->string n)))
 '- '(number number) (lambda (a b) (- a b))
 '- '(number) (lambda (a) (- a))
)

(apply-generic-log-tree)

(log "(+ n n) —> " (apply-generic-lookup '+ 'number 'number))
(log "(- n) —> " (apply-generic-lookup '- 'number))
(log "(+ s n) —> " (apply-generic-lookup '+ 'string 'number))
(log "(+ s s) —> " (apply-generic-lookup '+ 'string 'string))

(define tagged-abc (apply-generic-tag 'string "abc"))
(log "abc + 123 = " (apply-generic '+ tagged-abc tagged-123))


(define search (make-apply-generic-search apply-generic-global))

; Dummy registrations required for search tests:
(apply-generic-register
 '+ '(number string) +
 '+ '(number number number) +
 '+ '(number string number) +
 '+ '(number number string number) +
)

(log "\nSearch for +(...) ops: ")
(for-each log (search '+ '()))

(log "\nSearch for +(number ...) ops: ")
(for-each log (search '+ '(number)))

(log "\nSearch for +(number string ...) ops: ")
(for-each log (search '+ '(number string)))

(log "\nSearch for +(number number ...) ops: ")
(for-each log (search '+ '(number number)))

(log "\nSearch for +(number number number ...) ops: ")
(for-each log (search '+ '(number number number)))

(log "\nSearch for +(number number number number ...) ops: ")
(for-each log (search '+ '(number number number number)))

(log "\nSearch for +(number number string number ...) ops: ")
(for-each log (search '+ '(number number string number)))
