; Depends on: «table.scm», «../3.3.3/tree-red-black.scm»,
; «../2.5.3/index-tree.scm».

; Creates limited cache that implements table interface.
; Cache maker takes (limit on-prune) arguments. To prune
; keys over limit, it follows LRU (last recently used)
; strategy. If on-prune callback is given, it's called
; with (cache key value) arguments.
;
; Cache lookup hit does not alter the LRU index, to
; touch it, just add the same value by the same key.
;
; Cache add returns #t if actually added (size++).
;
; Cache remove returns the value removed, or void.
;
; In cache, iteration order is the LRU index order:
; the oldest item (first to prune) comes first.
;
; Note: that cache is one-level table, and all methods
; taking keys support single key argument!
;
(define (make-table-cache keys-eq?)

 ; Table ops «class» used to store data
 ; as (key -> (value . uid)) mapping.
 (define Table (make-table keys-eq?))

 (define new-table (table-op-make Table))
 (define table? (table-op-table? Table))
 (define lookup (table-op-lookup Table))
 (define add (table-op-add Table))
 (define remove (table-op-remove Table))

 (define (table cache)  (list-ref cache 0))
 (define (iget cache)   (list-ref cache 1))
 (define (ifirst cache) (list-ref cache 2))
 (define (iadd cache)   (list-ref cache 3))
 (define (idel cache)   (list-ref cache 4))
 (define (size cache)   (list-ref cache 5))
 (define (limit cache)  (list-ref cache 6))
 (define (iiter cache)  (list-ref cache 9))

 ; Returns current uid of the cache, then increments it.
 (define (next-uid cache)
  (define uid (list-ref cache 7))
  (list-set! cache 7 (+ 1 uid))
  uid
 )

 (define (on-prune cache key value)
  (define cb (list-ref cache 8))
  (if (procedure? cb) (cb cache key value) void)
 )

 (define (inc-size cache)
  (define s (size cache))
  (define l (limit cache))

  (if (>= s l)
   (error "Cache limit overflow!" s)
   (list-set! cache 5 (+ s 1))
  )

  (+ 1 s) ;<— return the new size
 )

 (define (dec-size cache)
  (define s (size cache))

  (if (>= 0 s)
   (error "Cache size is zero!" s)
   (list-set! cache 5 (- s 1))
  )

  (- s 1) ;<— return the new size
 )

 ; Create cache instance.
 (define (make-cache limit on-prune)

  ; Index tree used to store the priorities. It stores
  ; (uid —> key), where uid is cache-unique integer number
  ; incremented on each priority change of the related key.
  (define itree (make-index-tree))

  (list
   (new-table)             ; 0 table instance
   (index-tree-get itree)  ; 1 uid tree get
   (make-ifirst itree)     ; 2 ... get first (uid . key)
   (index-tree-set itree)  ; 3 ... add
   (index-tree-del itree)  ; 4 ... delete
   0                       ; 5 current size
   limit                   ; 6 fixed limit
   0                       ; 7 current uid
   on-prune                ; 8 on-prune callback
   (index-tree-iter itree) ; 9 index iter
  )
 )

 (define (make-ifirst itree)
  (define iter (index-tree-iter itree))
  (define (break uid key) key)
  (lambda () (iter break))
 )

 (define (cache? x)
  (and (pair? x) (table? (car x)))
 )

 (define (check x)
  (if (cache? x) x
   (error "A cache argument expected!" x)
  )
 )

 (define (cache-lookup cache key)
  (define vu (lookup (table (check cache)) key))
  (if (eq? void vu) void (car vu))
 )

 (define (limited? cache)
  (>= (size cache) (limit cache))
 )

 ; Touch existing (value . uid) record by generating
 ; the next uid and updating it in the index tree.
 (define (touch-existing cache key vu value)
  (define uid (next-uid (check cache)))

  ; Remove obsolete uid from the index:
  ((idel cache) (cdr vu))

  ; Add greater index (to be the last):
  ((iadd cache) uid key)

  ; Update value, uid directly in the record:
  (set-car! vu value)
  (set-cdr! vu uid)
  #f ;<— result of the same size
 )

 (define (assert-vu cache key uid)
  (define vu (lookup (table cache) key))

  (if
   (or
    (not (pair? vu))
    (not (= uid (cdr vu)))
   )
   (error "Cache integrity failed!" uid key)
   vu ;<— return checked (value . uid)
  )
 )

 (define (replace-oldest cache key value)
  ; Find the first uid in the index, it's the smallest
  ; value, and the oldest added or touched:
  (define xuid ((ifirst cache)))

  ; Get the key mapped by this uid:
  (define xkey ((iget cache) xuid))

  ; And the value by this key:
  (define xvu (assert-vu cache xkey xuid))

  ; Generate new uid:
  (define uid (next-uid cache))

  ; Remove oldest record:
  ((idel cache) xuid) ;<— from the index
  (remove (table cache) xkey) ;<— from the table

  ; Add the new one:
  ((iadd cache) uid key) ;<— to the index
  (add (table cache) (cons value uid) key)

  ; Invoke prune callback:
  (on-prune cache xkey (car xvu))
  #f ;<— result of the same size
 )

 (define (add-new cache key value)
  (define uid (next-uid cache))

  (inc-size cache)
  ((iadd cache) uid key)
  (add (table cache) (cons value uid) key)
  #t ;<— result of real add
 )

 (define (cache-add cache key value)
  (define vu (lookup (table (check cache)) key))

  (if (eq? void vu)
   (if (limited? cache)
    ; Prune and replace oldest record:
    (replace-oldest cache key value)
    ; Add new record and change the size:
    (add-new cache key value)
   )
   ; Update existing record by the key:
   (touch-existing cache key vu value)
  )
 )

 (define (cache-remove cache key)
  (define vu (lookup (table (check cache)) key))

  (if (eq? void vu) void
   (begin
    ((idel cache) (cdr vu)) ;<— from the index
    (remove (table cache) key) ;<— from the table
    (dec-size cache) ;<— decrement the size
    (car vu) ;<— resulting value
   )
  )
 )

 (define (cache-size cache)
  (size (check cache))
 )

 (define (cache-clear cache)
  (define itree (make-index-tree))
  (check cache)

  (list-set! cache 0 (new-table))             ; 0 table instance
  (list-set! cache 1 (index-tree-get itree))  ; 1 uid tree get
  (list-set! cache 2 (make-ifirst itree))     ; 2 ... get first
  (list-set! cache 3 (index-tree-set itree))  ; 3 ... add
  (list-set! cache 4 (index-tree-del itree))  ; 4 ... delete
  (list-set! cache 5 0)                       ; 5 current size
  (list-set! cache 7 0)                       ; 7 current uid
  (list-set! cache 9 (index-tree-iter itree)) ; 9 index iter
 )

 (define (cache-iterate cache visitor)
  (define t (table (check cache)))

  ((iiter cache)
   (lambda (uid key)
    (visitor key (car (lookup t key)))
   )
  )
 )


 (list ;<— resulting list of the ops
  make-cache     ; 0
  cache?         ; 1
  cache-lookup   ; 2
  cache-add      ; 3
  cache-remove   ; 4
  cache-size     ; 5
  cache-clear    ; 6
  cache-iterate  ; 7
 )
)
