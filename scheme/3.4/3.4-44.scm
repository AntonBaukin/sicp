(include "bank-account.scm")

(define (log . args) (for-each display args) (newline))

(define (logw . args)
 (define t (thread-name (current-thread)))
 (apply log (append (list (ts) " [§" t "] ") args))
)

; In «3.4-41.scm» we displayed synchronization issue
; of non-repeatable read when calculating totals via
; combinations of μ-ops. Here we display the same case
; for real multiprocessing with sleeping threads: we
; choose deposit-withdraw delayes so, our transfers
; meet the desired issue.

(define (transfer from to amount)
 (if (account-serial from 'withdraw amount)
  (account-serial to 'deposit amount)
 )
)

(define A (make-account "A" log 100 2 4))
(define B (make-account "B" log 0 2 4))

; This thread transfers 75 from A —> B.
(define (worker-a)
 (define (task)
  (transfer A B 75)
 )

 (define thread (make-thread task "a"))
 (thread-start! thread)
 thread
)

; This thread calculates A, B accounts total.
(define (worker-b)
 (define (task)
  (define balance-a 0)
  (define balance-b 0)

  (thread-sleep! 2)
  (logw "done delay 2sec")

  (set! balance-a ((A 'balance)))
  (logw "balance @A = " balance-a)

  (thread-sleep! 6)
  (logw "done delay 6sec")

  (set! balance-b ((B 'balance)))
  (logw "balance @B = " balance-b)

  ; We see total being 175. To solve this problem using
  ; locks, we must first lock ahead all the accounts,
  ; then calc the total, then release the locks.
  ;
  ; See task 48 that replays this scenario.

  (logw "total = " (+ balance-a balance-b))
 )

 (define thread (make-thread task "b"))
 (thread-start! thread)
 thread
)

(define a (worker-a))
(define b (worker-b))

(thread-join! a)
(thread-join! b)
