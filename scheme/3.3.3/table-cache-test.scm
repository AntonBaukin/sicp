(include "../3.3.2/assert.scm")
(include "../3.1/enumerate.scm")
(include "../3.3.3/tree-red-black.scm")
(include "../2.5.3/index-tree.scm")
(include "table.scm")
(include "table-cache.scm")

(define (log . args) (for-each display args) (newline))

(define Cache (make-table-cache eq?))
(define cache-make    (table-op-make Cache))
(define cache?        (table-op-table? Cache))
(define cache-size    (table-op-size Cache))
(define cache-add     (table-op-add Cache))
(define cache-remove  (table-op-remove Cache))
(define cache-lookup  (table-op-lookup Cache))
(define cache-iterate (table-op-iterate Cache))
(define cache-clear   (table-op-clear Cache))

(define (test limit)
 (define iprune 0)

 ; Here we check the order the items were pruned:
 (define (on-prune cache key value)
  (assert-equal?
   value (number->string key)
   "Wrong pruned value for key!"
  )

  (assert-eq? iprune key
   "Wrong prune order!"
  )

  (set! iprune (+ iprune 1))
 )

 (define cache (cache-make limit on-prune))

 (define (test-cache)
  (assert-true? (cache? cache))
  (assert-true? (not (cache? (make-table eq?))))
  (assert-eq? 0 (cache-size cache))

  ; Add initial [0 .. limit-1] items:
  (for-each
   (lambda (i)
    (assert-true?
     (cache-add cache (number->string i) i)
     "Item was not added to cache!" i
    )
   )
   (enumerate-n limit)
  )

  (test-content cache 0 limit)

  ; Add next [limit .. 2limit-1] items with prune:
  (for-each
   (lambda (i)
    (assert-true?
     (not (cache-add cache (number->string i) i))
     "Item was not prune-added to cache!" i
    )
   )
   (enumerate-range limit (- (* limit 2) 1))
  )

  (test-content cache limit (* limit 2))

  ; Remove the latest item:
  (assert-equal? (number->string limit) (cache-remove cache limit))
  (assert-eq? (- limit 1) (cache-size cache))

  ; Add it back:
  (assert-true? (cache-add cache (number->string limit) limit))
  (assert-eq? limit (cache-size cache))

  ; Touch all key except key = limit:
  (if (= 1 limit) void
   (for-each
    (lambda (i)
     (assert-true?
      (not (cache-add cache (number->string i) i))
      "Item was not touched in cache!" i
     )
    )
    (enumerate-range (+ limit 1) (- (* limit 2) 1))
   )
  )

  (test-content cache limit (* limit 2))
 )

 (test-cache) ;<— run initial test

 ; Clear cache, reset prune index, and run again:
 (cache-clear cache)
 (set! iprune 0)
 (test-cache)
)

; Checks that cache contains keys [b .. e-1] and
; the values being strings of that keys.
(define (test-range cache b e)
 (for-each
  (lambda (i)
   (assert-equal?
    (number->string i)
    (cache-lookup cache i)
    "Cache miss!"
   )
  )
  (enumerate-range b (- e 1))
 )
)

; Tests the range [e-1 .. b] via the cache iteration.
; Hint: items are iterated in the LRU index order.
(define (test-iter cache b e)
 (define index b)

 (cache-iterate cache
  (lambda (key value)
   (assert-eq?
    index key
    "Cache iterate wrong key!"
   )

   (assert-equal?
    (number->string index) value
    "Cache iterate wrong value!"
   )

   (set! index (+ index 1))
   void ;<— iterate all
  )
 )

 (assert-eq? e index "Cache iterate wrong size!")
)

(define (test-content cache b e)
 (assert-eq?
  (- e b)
  (cache-size cache)
  "Cache size mismatch!"
 )

 (test-range cache b e)
 (test-iter cache b e)
)

; Run tests for limit [1 ... 100]
(map test (enumerate-range 1 100))
