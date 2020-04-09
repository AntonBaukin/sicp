; This file is included before the last «eval-impl-set.scm»,
; after else «eval-impl-*.scm».
;
; It adds procedures treated as «primitives», but they are just
; our language' pre-defined ones.
;
; Note: that in Gambit Scheme we can not refer special
; forms or macroses as a procedures, and we have to
; support on the evaluator level.
;
(define eval-primes
 (list
  ; This name is defined in the private scope of eval
  ; implementation. It refers the top-level environment
  ; used as the default.
  global-env

  '+               +
  '-               -
  '*               *
  '/               /
  '=               =
  '<               <
  '>               >
  '<=              <=
  '>=              >=
  'car             car
  'cdr             cdr
  'cons            cons
  'list            list
  'not             not
  'null?           null?
  'pair?           pair?
  'list?           list?
  'length          length
  'append          append
  'reverse         reverse
  'assoc           assoc
  'eq?             eq?
  'equal?          equal?
  'symbol?         symbol?
  'procedure?      procedure?
  'string?         string?
  'string-append   string-append
  'number->string  number->string
  'error           error
  'caar            caar
  'cdar            cdar
  'cadr            cadr
  'cddr            cddr
  'cadar           cadar
  'caddr           caddr
  'cddar           cddar
  'cdddr           cdddr
  'caddar          caddar
 )
)
