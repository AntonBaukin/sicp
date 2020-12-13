(include "../2.5.1/defined.scm")
(include "eval-maker.scm")

; Standard modules of QEval implementation:
(define qeval-includes-std '(
 "../2.3.3/curry.scm"
 "../3.3.2/iterate.scm"
 "../4.4.4/interfaces.scm"
 "../4.4.4/defs.scm"
 "../4.4.4/stream.scm"
 "../4.4.4/utilities.scm"
 "../4.4.4/qeval-proc.scm"
 "../4.4.4/qeval-disp.scm"
 "../4.4.4/matching.scm"
 "../4.4.4/streams-db.scm"
 "../4.4.4/assertions.scm"
 "../4.4.4/rules.scm"
 "../4.4.4/qeval-body.scm"
))

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

; Adds the list of statements (assertions) to the given QEval instance.
(define (qeval-add-statements qeval statements)
 (for-each (qeval-add qeval) statements)
)
