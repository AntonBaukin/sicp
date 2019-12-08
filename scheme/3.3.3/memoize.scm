; Depends on the following files:
; «../3.3.3/tree-red-black.scm»
; «../2.5.3/index-tree.scm»
; «../3.3.3/table.scm»
; «../3.3.3/table-cache.scm»
;
; These files are for the default cache created when
; limit number is given to memoize function.
;


; Memoize is classical high-order function of functional
; programming. This implementation incapsulates limited
; cache table. The arguments are:
;  1 (Cache limit f)
;  2 (Cache cache f)
;  3 (limit [keys-eq?] f)
;
; «Cache» means a list of table interface ops. It may be
; any table, but a limited cache is required not to leak
; the memory. Cache instance is made with the given limit.
;
; You may also give «cache» instance directly, but «Cache»
; ops list is also required.
;
; Third variant builds table cache with the given limit
; and optional «keys-eq?» predicate.
;
(define (memoize . args)
 (define Cache
  (if (number? (car args))
   (begin
    ; Create default cache on first demand:
    (if (null? MemoizeDefautCache)
     (begin
      (set! MemoizeDefautCache
       (make-table-cache memoize-keys-eq?)
      )
      MemoizeDefautCache
     )
     MemoizeDefautCache
    )
   )
   (car args) ;<— Cache is given directly
  )
 )

 (define cache
  (cond
   ((number? (cadr args)) ;<— only 1) may be
    ((table-op-make Cache) (cadr args))
   )

   ((= 2 (length args)) ;<— only 3) short may be
    ((table-op-make Cache) (car args))
   )

   ((procedure? (cadr args)) ;<— only 3) long may be
    ((table-op-make Cache) (car args) (cadr args))
   )

   (else (cadr args)) ;<— suppose 2) variant
  )
 )

 (define save (table-op-add Cache))
 (define lookup (table-op-lookup Cache))

 (define f (if (= 2 (length args)) (cadr args) (caddr args)))

 (define (mem . args)
  (define v (lookup cache args))

  ; Found cached value?
  (if (not (eq? void v)) v
   (let ((v (apply f args)))
    (save cache v args) ;<— cache new value
    v
   )
  )
 )

 (if (procedure? f) void
  (error "Not a function is given to memoize!" f)
 )

 (if ((table-op-table? Cache) cache) void
  (error "Not a table is made for memoize!" cache)
 )

 mem ;<— resulting (memoized) function
)

; Default «keys-eq?» predicate for argitrary key lists
; that uses «eq?» to test the items.
(define (memoize-keys-eq? keys-a keys-b)
 (cond
  ((and (null? keys-a) (null? keys-b))
   #t
  )

  ((and (null? keys-a) (not (null? keys-b)))
   #f
  )

  ((and (not (null? keys-a)) (null? keys-b))
   #f
  )

  ((and (not (eq? (car keys-a) (car keys-b))))
   #f
  )

  (else (memoize-keys-eq? (cdr keys-a) (cdr keys-b)))
 )
)

; Default «Cache» opts are created on first demand.
(define MemoizeDefautCache '())
