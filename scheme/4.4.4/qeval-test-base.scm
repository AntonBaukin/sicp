(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "../4.4.4/Microshaft.scm")
(include "../4.4.4/qeval.scm")

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

(define (add-rule-impl qeval conclusion body-list)
 ((qeval-rule qeval)
  (if (= 0 (length body-list))
   (list 'rule conclusion)
   (list 'rule conclusion (car body-list))
  )
 )
)

(define-macro (add-rule conclusion . body)
 `(add-rule-impl qeval (quote ,conclusion) (quote ,body))
)
