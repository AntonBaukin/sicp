(include "../2.5.1/defined.scm")
(include "eval-maker.scm")
(include "qeval-includes-std.scm")

; User defined QEval modules, or the standard:
(define-value-if-not 'qeval-includes qeval-includes-std)

; Makes Queries Evaluator. Returns a «qeval» instance
; being a list of functions accessed via «qeval-*».
(define (make-qeval)
 (eval-maker qeval-includes 'make-qeval-body)
)

; Returns a function that adds a statement to the database.
; Resulting function arguments: (statement).
(define (qeval-add qeval)
 (list-ref qeval 0)
)

; Returns a function that adds a rule to the database.
; Resulting function arguments: (rule).
(define (qeval-rule qeval)
 (list-ref qeval 1)
)

; Returns a QEval query function.
; Resulting function arguments: (query), returns a list of answers.
(define (qeval-query qeval)
 (list-ref qeval 2)
)

; Returns a QEval query function with mapping.
; Resulting function arguments: (mapper query).
; Mapper takes: (frame).
(define (qeval-query-map qeval)
 (list-ref qeval 3)
)

; Adds the list of statements (assertions) to the given QEval instance.
(define (qeval-add-statements qeval statements)
 (for-each (qeval-add qeval) statements)
)
