(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "../4.4.4/qeval.scm")

; Make instance of QEval.
(define qeval (make-qeval))
(define query (qeval-query qeval))
(define query-map (qeval-query-map qeval))

;
; Test support macroses.
;
(define (log-lines items)
 (for-each
  (lambda (item)
   (display item)
   (display "\n")
  )
  items
 )
)

(define-macro (log-query query)
 `(log-lines (query (quote ,query)))
)

(define-macro (log-query-map mapper query)
 `(log-lines (query-map ,mapper (quote ,query)))
)

(define-macro (test-query query . items)
 `(assert-equal? '(,@items) (query (quote ,query)))
)

(define (test-and-log-impl request items)
 (define results (query request))
 (assert-equal? items results)
 (log-lines results)
)

(define-macro (test-and-log query . items)
 `(test-and-log-impl (quote ,query) '(,@items))
)

(define (test-and-log-map-impl mapper request items)
 (define results (query-map mapper request))
 (assert-equal? items results)
 (log-lines results)
)

(define-macro (test-and-log-map mapper query . items)
 `(test-and-log-map-impl ,mapper (quote ,query) '(,@items))
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

(define (frame-get frame name)
 (define (find-binding bindings name)
  (cond
   ((null? bindings) '())
   ((eq? name (caar bindings)) (car bindings))
   (else (find-binding (cdr bindings) name))
  )
 )

 (define b (find-binding (cdr frame) name))
 (if (null? b) '() (cdr b))
)
