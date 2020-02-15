(define (log . args) (for-each display args) (newline))

(define (no-operands? exps)
 (null? exps)
)

(define (first-operand exps)
 (car exps)
)

(define (rest-operand exps)
 (cdr exps)
)

(define (list-of-values-left exps)
 (if (no-operands? exps) '()
  (let ((value (eval (first-operand exps))))
   (cons value (list-of-values-left (rest-operand exps)))
  )
 )
)

(define (list-of-values-right exps)
 (if (no-operands? exps) '()
  (let ((rest (list-of-values-right (rest-operand exps))))
   (cons (eval (first-operand exps)) rest)
  )
 )
)

(define inc-value 0)

; Expresson with side effect to track the order:
(define (inc)
 (define result inc-value)
 (set! inc-value (+ inc-value 1))
 result
)

(log "Left evaluation: "
 (list-of-values-left '((inc) (inc) (inc) (inc) (inc)))
)

(log "Right evaluation: "
 (list-of-values-right '((inc) (inc) (inc) (inc) (inc)))
)
