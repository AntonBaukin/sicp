;
; Here are difinitions for SICP Chapter 4 evaluator.
; They are included into closure environment of eval
; created in «make-eval-impl».
;
; It seems that this is the simplest way how to inject
; expression into existing closure in Gambit Scheme — as
; evaluation in the given environment is not supported.
;
; Warning! This implementation just resembles SICPS',
; but not exactly the same! The main difference is that
; it uses red-black tree tables for the frames, thus
; there is no mess with the lists here.
;
; More over. Not to overwrite everithing for single tasks,
; we created this evaluator to be modular of several files
; included — this file is the first of them.
;
(define (eval-basic exp env)
 ;(log "EVAL " exp)

 (cond
  ((self-evaluating? exp)
   exp
  )

  ((variable? exp)
   (lookup-variable exp env)
  )

  ((quoted? exp)
   (text-of-quotation exp)
  )

  ((definition? exp)
   (eval-definition exp env)
  )

  ((application? exp)
   (apply-impl
    (eval-impl (operator exp) env)
    (list-of-values (operands exp) env)
   )
  )
 )
)
