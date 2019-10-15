
(define (make-queue-ops)
 (include "iterate.scm")

 (define null '())

 (define (make-empty)
  (cons null null)
 )

 (define (last-pair lst)
  (if (null? (cdr lst)) lst
   (last-pair (cdr lst))
  )
 )

 ; Creates queue from a list.
 (define (make-from lst)
  (if (null? lst) (make-empty)
   (cons lst (last-pair lst))
  )
 )

 (define (empty? queue)
  (null? (car queue))
 )

 ; Adds item to the tail of the queue.
 (define (append queue item)
  (define p (cons item null))

  (if (empty? queue)
   (set-car! queue p)
   (set-cdr! (cdr queue) p)
  )

  (set-cdr! queue p)
 )

 (define (check-empty queue)
  (if (empty? queue)
   (error "The queue is empty")
   void
  )
 )

 ; Removes the queue's first item and returns it.
 (define (pop queue)
  (check-empty queue)
  (let ((first (car queue)))
   (set-car! queue (cdr first))
   ; The queue became empty? Set the last pointer:
   (if (null? (cdr first)) (set-cdr! queue null) void)
   (car first) ;<— the value
  )
 )

 (define (first queue)
  (check-empty queue)
  (caar queue)
 )

 (define (last queue)
  (check-empty queue)
  (cadr queue)
 )

 ; Adds item to be the head of the queue.
 (define (push queue item)
  (define p (cons item (car queue)))

  (set-car! queue p)
  (if (empty? queue) (set-cdr! queue p) void)

  queue ;<— as in SICP, we return queue from !-op
 )

 (define (iterate queue visitor)
  (iterate-list (car queue) visitor)
 )
 
 (define (qlength queue)
  (length (car queue))
 )

 
 (list
  make-empty   ; 0
  make-from    ; 1
  empty?       ; 2
  append       ; 3
  pop          ; 4
  first        ; 5
  last         ; 6
  push         ; 7
  iterate      ; 8
  qlength      ; 9
 )
)
