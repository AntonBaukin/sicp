

; Finds all permutations of the given items.
; Invokes the callback for each one, the order
; is reversed — initial sequence comes the last.
(define (for-each-permutation items taker)
 (define (next head tail res)
  (if (null? tail)
   (if (null? head) (taker res))
   (begin
    (next
     '()
     (append (reverse head) (cdr tail))
     (cons (car tail) res)
    )

    (next
     (cons (car tail) head)
     (cdr tail)
     res
    )
   )
  )
 )
 
 (next '() items '())
)

; Returns a list of all permutations of the items.
(define (permutations items)
 (define result '())

 (for-each-permutation items
  (lambda (items)
   (set! result (cons items result))
  )
 )

 result
)


; Takes a list of lists of various size.
; Invokes «taker» giving it each combination
; of all items that do preserve the order.
(define (for-each-order-shuffle items taker)
 (define (empty? items)
  (cond
   ((null? items) #t)
   ((not (null? (car items))) #f)
   (else (empty? (cdr items)))
  )
 )

 ; Invokes callback with (result other-items),
 ; where other items is a list of lists having
 ; the result's first item removed.
 (define (for-each-item head tail res)
  (if (not (null? tail))
   (begin
    (if (not (null? (car tail)))
     (next
      (cons (caar tail) res) ;<— result items
      (append
       (reverse ;<– form the same list with the head excluded
        (cons
         (cdar tail) ;<— exclude first item from current head
         head
        )
       )
       (cdr tail)
      )
     )
    )

    (for-each-item (cons (car tail) head) (cdr tail) res)
   )
  )
 )

 (define (next res items)
  (if (empty? items)
   (taker (reverse res))
   (for-each-item '() items res)
  )
 )

 (for-each-item '() items '())
)


; Returns a list of all order-preserving
; permutations of the items.
(define (order-shuffle items)
 (define result '())

 (for-each-order-shuffle items
  (lambda (items)
   (set! result (cons items result))
  )
 )

 (reverse result)
)


; Makes permutations tests of ops as follows.
; Argument «ops» is a list of ops being procedures
; treated as atomic. Tester makes all permutations
; of the ops order and invokes: «reset» first, then
; permutated ops, finally «done».
(define (test-permutations reset done ops)
 (for-each
  (lambda (ops)
   (reset)
   (for-each (lambda (op) (op)) ops)
   (done)
  )
  (permutations ops)
 )
)

; Makes combination tests of micro-ops as follows.
; Argument «ops» is a list of lists of a micro-op
; being procedure treated as atomic. Tester makes
; all shuffles presering the ops order and invokes:
; «reset» first, then shuffled ops, finally «done».
(define (test-shuffles reset done ops)
 (for-each
  (lambda (ops)
   (reset)
   (for-each (lambda (op) (op)) ops)
   (done)
  )
  (order-shuffle ops)
 )
)
