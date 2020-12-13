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
;(test-query
; (job ?name ?position)
;; —————————————————————————————————————————————————————————
; (job (Aull DeWitt) (administration secretary))
; (job (Cratchet Robert) (accounting scrivener))
; (job (Scrooge Eben) (accounting chief accountant))
; (job (Warbucks Oliver) (administration big wheel))
; (job (Doom Hugo) (computer programmer trainee))
; (job (Tweakit Lem E) (computer technician))
; (job (Fect Cy D) (computer programmer))
; (job (Hacker Alyssa P) (computer programmer))
; (job (Bitdiddle Ben) (computer wizard))
;)


; —————————————————————————————————————————————————————————
; —— Section Two: Test composite queries w/o rules       ——
; —————————————————————————————————————————————————————————

; «And» with two related conditions:
(test-query
 (and
  (job ?person (computer programmer))
  (address ?person ?where)
 )
; —————————————————————————————————————————————————————————
 (and
  (job (Fect Cy D) (computer programmer))
  (address (Fect Cy D) (Cambridge (Ames Street) 3))
 )
 (and
  (job (Hacker Alyssa P) (computer programmer))
  (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
 )
)
