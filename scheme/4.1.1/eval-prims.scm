;
; This file is included second, after «eval-impl.scm».
; It adds procedures treated as «promitives», but they
; just our language pre-defined ones.
;
; Note: that in Gambit Scheme we can not refer special
; forms or macroses as a procedures, and we have to
; support on the evaluator level.
;
(define-variables
 ; This name is define in private scope of eval implementation.
 ; It refers the top-level environment used as the default.
 global-env

 '+       +
 '-       -
 '*       *
 '/       /
 'car     car
 'cdr     cdr
 'cons    cons
 'list    list
 'null?   null?
 'pair?   pair?
 'list?   list?
 'append  append
 'map     map
)
