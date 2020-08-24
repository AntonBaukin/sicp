;
; This file contains test cases specific for amb-evaluator.
;
(eval-basic (debug on))

; Amb evaluator: stop on single value.
(assert-equal? (list void 123) (eval-amb-list 123))

; Amb evaluator: empty (amb).
(assert-eq? void (eval-basic (amb)))
(assert-equal? (list void) (eval-amb-list (amb)))

; Amb evaluator: single variant.
(assert-eq? 1 (eval-basic (amb 1)))
(assert-eq? 1 (eval-basic (amb 1 2)))
(assert-eq? 1 (eval-basic (amb 1 2 3)))
(assert-equal? (list void 1) (eval-amb-list (amb 1)))

; Amb evaluator: more variants.
(assert-equal? (list void 2 1) (eval-amb-list (amb 1 2)))
(assert-equal? (list void 3 2 1) (eval-amb-list (amb 1 2 3)))
(assert-equal? (list void 4 3 2 1) (eval-amb-list (amb 1 2 3 4)))
(assert-equal? (list void 5 4 3 2 1) (eval-amb-list (amb 1 2 3 4 5)))

; Amb evaluator: combinations of two lists (SICP sample).
(assert-equal?
 (list void '(3 b) '(3 a) '(2 b) '(2 a) '(1 b) '(1 a))
 (eval-amb-list (list (amb 1 2 3) (amb 'a 'b)))
)

; Amb evaluator: results of the last expression.
(assert-equal?
 (list void 6 5 4)
 (eval-amb-list
  (amb 1 2 3) ;<— this is ignored
  (amb 4 5 6)
 )
)

; Amb evaluator: recurse & require.
(assert-equal?
 (list void 3 2 1)
 (eval-amb-list
  (define (an-element-of items)
   (require (not (null? items)))
   (amb (car items) (an-element-of (cdr items)))
  )

  (an-element-of '(1 2 3))
 )
)

(log "Amb Evaluator of §4.3.1 successfully tested!" "\n")
