(include "../2.3.3/curry.scm")
(include "../2.3.3/tree-util-walk.scm")
(include "../2.3.3/tree-util-min.scm")
(include "../2.3.3/tree-util-next.scm")


; Makes scope search function that takes an operation
; symbol and variable number of argument type symbols.
; Search result is a list of all registrations for that
; operator and the operands starting with that types.
; Each item of the resulting list is a pair of: full
; list of types (it has the given types as a prefix);
; and the function registered.
;
(define (make-apply-generic-search scope)
 (define lookup (apply-generic-scope-lookup scope))

 ; Dummy function required for a generic key.
 (define noop (lambda () void))

 ; This internal call return the registration tree (root node).
 ; As the root always changes, we have to incoke it each time.
 (define get-scope-tree
  (apply-generic-scope-internal-get-tree scope)
 )

 ; Searches the next registration entry in the tree order.
 ; At the iteration end we reach the tree end, or go to
 ; else operation or arguments list with else types prefix.
 (define search-op-next (make-tree-get-next ApplyGenericScope))

 (define (types-prefix? prefix types)
  (cond
   ((null? prefix) #t)
   ((null? types)  #f)

   ((eq? (car prefix) (car types))
    (types-prefix? (cdr prefix) (cdr types))
   )

   (else #f) ;<— types mismatch
  )
 )

 (define (match? op arg-types entry)
  (and
   (eq? op (car entry)) ;<— the same operation?
   (types-prefix? arg-types (cadr entry))
  )
 )

 ; Takes the next entry and checks whether it matches the
 ; op + args prefix criteria. If so, continues.
 (define (search-iter op arg-types tree prev result)
  (let ((next (search-op-next tree prev)))
   (if (not (match? op arg-types next))
    result
    (search-iter op arg-types tree next
     ;—> we take the types and the function only:
     (cons (cons (cadr next) (caddr next)) result)
    )
   )
  )
 )

 (lambda (op arg-types)
  (let* (
    ; First, we check whether a call with exactly same types
    ; is registered. If so, we add this combination.
    (exact (lookup op arg-types))
    (result (if (null? exact) '()
     (list (cons arg-types exact))
    ))

    ; Current root of the scope registration tree.
    (tree (get-scope-tree))
    (prev (apply-generic-make-op-key op arg-types noop))
   )

   (reverse (search-iter op arg-types tree prev result))
  )
 )
)