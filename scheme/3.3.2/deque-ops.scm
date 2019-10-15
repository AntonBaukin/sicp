
; Here deque is implemented as bidirectional list.
; Forward direction forms a normal list. Instead
; of storing items in car, we save there a pair:
; (item . back reference).
;
; We append items to the tail of the deque, as
; it's done in a queue, and we pop items from
; the head. We also may push items to the head,
;
(define (make-deque-ops)
 (include "iterate.scm")

 (define null '())

 (define (make-empty)
  (cons null null)
 )

 ; Item of a deque, forms plain forward list.
 (define (make-triple item prev next)
  (cons (cons item prev) next)
 )

 ; Creates triples from a sourse list, returns the last.
 (define (copy-and-trace prev tail)
  (if (null? tail) prev
   (let ((cur (make-triple (car tail) prev null)))
    (set-cdr! prev cur)
    (copy-and-trace cur (cdr tail))
   )
  )
 )

 (define (make-from lst)
  (if (null? lst) (make-empty)
   (let ((first (make-triple (car lst) null null)))
    ; As in a queue we store (first . last) as a deque.
    (cons first (copy-and-trace first (cdr lst)))
   )
  )
 )

 (define (empty? deque)
  (null? (car deque))
 )

 ; Pretty much the same as in the queue...
 (define (append deque item)
  (define triple (make-triple item (cdr deque) null))

  (if (empty? deque)
   (set-car! deque triple)
   (set-cdr! (cdr deque) triple)
  )

  (set-cdr! deque triple)
 )
 
 (define (check-empty deque)
  (if (empty? deque)
   (error "The deque is empty")
   void
  )
 )

 (define (pop deque)
  (check-empty deque)
  (let ((first (car deque)))
   (set-car! deque (cdr first))
   ; The deque became empty?
   (if (null? (cdr first))
    (set-cdr! deque null)
    ; (caar deque) is (item . back) of the first triple
    (set-cdr! (caar deque) null)
   )
   (caar first) ;<— the value
  )
 )

 (define (first deque)
  (check-empty deque)
  (caaar deque)
 )

 (define (last deque)
  (check-empty deque)
  (caadr deque)
 )

 (define (push deque item)
  (define triple (make-triple item null (car deque)))

  (if (empty? deque)
   (set-cdr! deque triple)
   ; (caar deque) is (item . back) of the first triple
   (set-cdr! (caar deque) triple)
  )
  (set-car! deque triple)

  deque ;<— as in SICP, we return deque from !-op
 )

 (define (iterate deque visitor)
  (iterate-list (car deque) (lambda (x) (visitor (car x))))
 )

 (define (dlength deque)
  (length (car deque))
 )

 ; Removes the item from the tail of the queue
 ; (the last to pop). This is the only operation
 ; that actually requires back pointers.
 (define (take deque)
  (check-empty deque)
  (let ((last (cdr deque)))
   (set-cdr! deque (cdar last))
   ; The deque became empty?
   (if (null? (cdar last))
    (set-car! deque null)
    (set-cdr! (cdr deque) null)
   )
   (caar last) ;<— the value
  )
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
  dlength      ; 9
  take         ; 10
 )
)
