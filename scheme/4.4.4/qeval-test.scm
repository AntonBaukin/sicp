(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "Microshaft.scm")
(include "qeval.scm")


; Make instance of QEval and populate it with Microshaft database.
(define qeval (make-qeval))
(define query (qeval-query qeval))
(qeval-add-statements qeval Microshaft)

; Test support macroses:
(define-macro (log-query query)
 `(log (query (quote ,query)))
)

(define-macro (test-query query . items)
 `(assert-equal? '(,@items) (query (quote ,query)))
)


; —————————————————————————————————————————————————————————
; —— Section One: Test simple queries for the assertions ——
; —————————————————————————————————————————————————————————

; List all known persons with their positions:
(test-query
 (job ?name ?position)
; —————————————————————————————————————————————————————————
 (job (name Aull DeWitt) (position administration secretary))
 (job (name Cratchet Robert) (position accounting scrivener))
 (job (name Scrooge Eben) (position accounting chief accountant))
 (job (name Warbucks Oliver) (position administration big wheel))
 (job (name Doom Hugo) (position computer programmer trainee))
 (job (name Tweakit Lem E) (position computer technician))
 (job (name Fect Cy D) (position computer programmer))
 (job (name Hacker Alyssa P) (position computer programmer))
 (job (name Bitdiddle Ben) (position computer wizard))
)
