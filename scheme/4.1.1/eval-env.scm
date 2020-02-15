(include "../3.3.3/tree-red-black.scm")
(include "../3.3.3/table-tree.scm")


; «Class» (collection of ops) of environment frame table.
; The implementation is based on red-black tree table from §3.3.3.
(define EvalEnvFrame
 (make-table-tree
  (lambda (a b)
   (string<? (symbol->string a) (symbol->string b))
  )
 )
)

; This file defines the environment as a list of three fields:
;
; 0) 'environment tag;
; 1) list of frames used as a stack;
; 2) enclosing environment, or null.
;
; File «eval-impl.scm» contains all helping functions related
; to environments. This file is included on the eval level,
; i.e. on the level of underlying Scheme.
;
; When creating environment you may define top-level
; variables with the pairs: (name . value).
;
; Note that these definitions are not overwritten when
; installing primitive procedures, thus you may provide
; your own ones. See «eval-prims.scm».
;
(define (eval-make-env . definitions)
 (define frame ((table-op-make EvalEnvFrame)))
 (define frame-add (table-op-add EvalEnvFrame))

 (for-each
  (lambda (nv)
   ; Arguments (table value keys...):
   (frame-add frame (cdr nv) (car nv))
  )
  definitions
 )

 ; Resulting environment object:
 (list 'environment (list frame) '())
)

; Adds variable to the top frame of the environment.
; It differs from SICP's «define-variable» in the order
; of the arguments. We added this ability to the level
; of the underlying system not to restrict creating
; initial (global) environment of the evaluator.
(define (eval-env-define env var-name-symbol value)
 ((table-op-add EvalEnvFrame)
  (car (list-ref env 1))
  value
  var-name-symbol
 )
)
