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
; 2) enclosing environment, or null;
; 3) additional info (mostly for debug), see «eval-env-info».
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
   ; Arguments are (table value keys...):
   (frame-add frame (cdr nv) (car nv))
  )
  definitions
 )

 ; Resulting environment object:
 (list 'environment (list frame) '() (list 0 (eval-env-gen-uid)))
)

; Extends environment. Integer level starts with 0 for the
; root environment. When extending, level is incremeneted.
; All procedures in the info-list are invoked passing info
; as single argument, resulting info is expected.
(define (eval-extend-env env)
 (define frame ((table-op-make EvalEnvFrame)))

 ; Resulting environment object:
 (list 'environment (list frame) env (eval-extend-env-info env))
)

; Creates a copy of the given environmant placing additional
; frame to the top of the stack.
(define (eval-nest-env env)
 (define frame ((table-op-make EvalEnvFrame)))
 (define result (list-copy env))

 (set-car! (cdr result) (cons frame (list-ref env 1)))
 result ;<— resulting environment object
)

; Stores incremented index of environments created.
(define eval-env-uid 0)

; Generates string env-uid-${index}.
(define (eval-env-gen-uid)
 (define uid (string-append "env-uid-" (number->string eval-env-uid)))
 (set! eval-env-uid (+ 1 eval-env-uid)) ;<— increment the index
 uid
)

(define (eval-extend-env-info env)
 (define info0 (list-ref env 3))
 (define info1 (list (+ 1 (car info0)) (eval-env-gen-uid)))

 (define (next info0 info1)
  (cond
   ((null? info0) info1)
   ((procedure? (car info0))
    (next (cdr info0) ((car info0) info1))
   )
   (else (next (cdr info0) info1))
  )
 )

 (next info0 info1)
)

; Checks that given environments are the same by
; comparing on equality UIDs at index-1 of info
(define (eval-env-eq? env-a env-b)
 (define info-a (list-ref env-a 3))
 (define info-b (list-ref env-b 3))
 (equal? (cadr info-a) (cadr info-b))
)

; Debug info assigned to each environment is a list
; of: (level ... other data ...). Integer level starts
; with 0 for the root environment. When extending, level
; is incremeneted, see «eval-extend-env».
(define (eval-env-info env)
 (if (null? env) '() (list-ref env 3))
)

(define (eval-env-info-set env info)
 (if (not (list? info))
  (error "Eval set environment info got not a list" info)
 )

 (if (not (number? (car info)))
  (error "Eval set environment info without the level" info)
 )

 (set-car! (cdddr env) info)
 env ;<— return same environment
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
