(include "apply-generic-utils.scm")

; Checks that the giben object is a tag wrapping pair
; having the given symbol tag as the first item.
; Compare it with a havier version of §2.4.3.
(define (apply-generic-tagged? tag obj)
 (and
  (symbol? tag)
  (pair? obj)
  (eq? tag (car obj))
 )
)

; Wraps given object into a pair with the given tag symbol.
(define (apply-generic-tag tag obj)
 (if (symbol? tag)
  (cons tag obj)
  (error "Not a tag symbol:" tag)
 )
)

; «Type» of a Scope being the functions lookup tree.
(define ApplyGenericScope (apply-generic-make-scope-type))

; Second implementation of apply-generic function
; that now supports denstinct storages.
;
; Returns a list of the functions:
; – apply-generic function bound to the scope;
; – register (put) function for the scope;
; - lookup in the scope by the signature;
; – get the scope tree (for the tests).
;
; Functions to check or wrap a symbol tag remain
; global as they do not depend on a scope.
;
; Takes three external strategies:
; – tag-get() that returns tag symbol from the given wrapping
;   pair, defaults to apply-generic-tag-get();
; – unwrap() that returns object wrapped by a tag procedure,
;   defaults to apply-generic-unwrap();
; – apply fallback that is invoked when there is no direct match
;   in the table and must return. The default raises error: see
;   apply-generic-fallback-error().
;
; These strategies allow to customize the wrapping procedure
; according to some exercises, such as not wrapping numbers.
;
(define (apply-generic-make tag-get unwrap apply-fallback)
 (define scope->list (tree-op->list ApplyGenericScope))
 (define scope<-list (tree-op<-list ApplyGenericScope))
 (define scope-search (tree-op-search ApplyGenericScope))

 ; Local scope tree instance.
 (define scope '())

 ; Recreates scope tree on each insert.
 (define (put key)
  (let* (
    (old (scope->list scope))
    (new (scope<-list (cons key old)))
   )
   (set! scope new)
  )
 )

 (define (msg-key)
  (string-append msg ": " (apply-generic-op-key->str key))
 )

 ; Put with the check for the existing entry.
 (define (put-safe key)
  (if (null? (scope-search scope key)) (put key)
   (error (msg-key "Key is already registered for apply-generic" key))
  )
 )

 ; Finds a function registered by op symbol and the
 ; arguments symbols list. Returns '() on not found.
 (define (lookup op-symbol arg-symbols-list)
  (let* ( ; Search by the key with irrelevant function
    (key (apply-generic-make-op-key op-symbol arg-symbols-list -))
    (res (scope-search scope key))
   )
   (if (null? res) res (caddr res))
  )
 )

 ; Operation symbol is an alias for a function name.
 ; In typed languages, C++ and Java, you may statically
 ; override a function name with else list of the args.
 ; In Lisp we dynamically dispatch different functions
 ; having the same symbolic alias by the tags of the args.
 ;
 (define (apply-generic op-symbol . args)
  (let* (
    (arg-symbols-list (map tag-get args))
    (arguments (map unwrap args))
    (function (lookup op-symbol arg-symbols-list))
   )
   (if (procedure? function)
    (apply function arguments)
    (apply-fallback op-symbol arg-symbols-list arguments)
   )
  )
 )

 ; Apply generic registration function. Takes arbitrary number
 ; of arguments: i) operation symbol, i+1) list of symbols
 ; being tags of a call arguments, i+2) function to call.
 ; 
 (define (register . op-args-func)
  (apply apply-generic-register-impl (cons put-safe op-args-func))
 )

 ; Scoped operations set.
 (list apply-generic register lookup (lambda () scope))
)

; Default apply fallback that raises an error.
(define (apply-generic-fallback-error op-symbol arg-symbols-list arguments)
 (error "Apply generic function is not found for: " op-symbol arg-symbols-list)
)

(define (apply-generic-make-default)
 (apply-generic-make
  apply-generic-tag-get
  apply-generic-unwrap
  apply-generic-fallback-error
 )
)

; Returns apply-generic function instance from the scope.
(define (apply-generic-scope-function scope)
 (car scope)
)

; Returns apply-generic register function from the scope.
(define (apply-generic-scope-register scope)
 (cadr scope)
)

; Returns apply-generic scope lookup function.
(define (apply-generic-scope-lookup scope)
 (list-ref scope 2)
)

(define (apply-generic-scope-internal-get-tree scope)
 (list-ref scope 3)
)


; Global version of apply generic functions collection.
(define apply-generic-global (apply-generic-make-default))

; Overwrites the global scope, apply-generic supports this.
(define (apply-generic-global-reset)
 (set! apply-generic-global (apply-generic-make-default))
)

; Apply generic that works with the global scope.
(define (apply-generic op-symbol . args)
 (apply
  (apply-generic-scope-function apply-generic-global)
  (cons op-symbol args)
 )
)

; Register function for apply generic. For the notation
; of the arguments — see apply-generic-make() comments.
(define (apply-generic-register . op-args-func)
 (apply
  (apply-generic-scope-register apply-generic-global)
  op-args-func
 )
)

; Lookup function of the global scope.
(define (apply-generic-lookup op-symbol . arg-symbols)
 (apply
  (apply-generic-scope-lookup apply-generic-global)
  (list op-symbol arg-symbols)
 )
)
