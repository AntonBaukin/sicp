(include "../2.3.3/quick-sort.scm")
(include "bank-account.scm")

; Here we continue the transfers task 44 and apply mutual
; lock both to the transfer and to the totals calculation.

(define (log . args) (for-each display args) (newline))

(define (logw . args)
 (define t (thread-name (current-thread)))
 (apply log (append (list (ts) " [§" t "] ") args))
)

(define (locker account task)
 ((account 'serializer)
  (lambda ()
   (logw "lock account @" (account 'id))
   (task)
  )
 )
)

(define (transfer from to amount)
 (if ((from 'withdraw) amount)
  ((to 'deposit) amount)
 )
)

(define A (make-account "A" log 100 2 4))
(define B (make-account "B" log 0 2 4))
(define C (make-account "C" log 0 2 4))

; This thread transfers 35 from A —> B, then 50 A —> C.
(define (worker-a)
 (define (task)
  (transfer A B 35)
  (transfer A C 50)
 )

 (define syn-task
  (mutual-lock (list C B A) locker task)
 )

 (define thread (make-thread syn-task "a"))
 (thread-start! thread)
 thread
)

; This thread transfers 20 from C —> B after 2sec delay.
(define (worker-b)
 (define (task)
  (transfer B C 20)
 )

 (define (delayed-task)
  (thread-sleep! 2)
  (logw "done delay 2sec")
  ((mutual-lock (list C B) locker task))
 )
 
 (define thread (make-thread delayed-task "b"))
 (thread-start! thread)
 thread
)

; This thread calculates A, B, C accounts total.
(define (worker-c)
 (define (task)
  (define balance-a 0)
  (define balance-b 0)
  (define balance-c 0)

  (set! balance-c ((C 'balance)))
  (logw "balance @C = " balance-c)

  (thread-sleep! 2)
  (logw "done delay 2sec")

  (set! balance-b ((B 'balance)))
  (logw "balance @B = " balance-b)

  (thread-sleep! 3)
  (logw "done delay 3sec")

  (set! balance-a ((A 'balance)))
  (logw "balance @A = " balance-a)

  (logw "total = " (+ balance-a balance-b balance-c))
 )

 (define (delayed-task)
  (thread-sleep! 3)
  (logw "done delay 3sec")
  
  ((mutual-lock (list B A C) locker task))
 )

 (define thread (make-thread delayed-task "c"))
 (thread-start! thread)
 thread
)

(define a (worker-a))
(define b (worker-b))
(define c (worker-c))

(thread-join! a)
(thread-join! b)
(thread-join! c)
