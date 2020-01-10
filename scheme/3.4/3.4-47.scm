(include "semaphore.scm")
(include "ts.scm")

(define (log . args) (for-each display args) (newline))

(define sem (make-semaphore 2))

(define (worker id timeout)
 (define (logx . args)
  (apply log (append (list (ts) " [§" id "] ") args))
 )

 (define (task)
  (logx "started...")
  (semaphore-acquire sem)
  (logx "acquired semaphore")
  (thread-sleep! timeout)
  (logx "woke after " timeout " seconds")
  (semaphore-release sem)
  (logx "released semaphore, exiting...")
 )

 ; Create and start worker thread:
 (define thread (make-thread task id))
 (thread-start! thread)
 thread
)

(define a (worker "A" 5))
(define b (worker "B" 6))
(define c (worker "C" 4))

(thread-join! a)
(thread-join! b)
(thread-join! c)

; Prints the following:
;
; 0.000 [§A] started...
; 0.000 [§A] acquired semaphore
; 0.000 [§B] started...
; 0.000 [§B] acquired semaphore
; 0.000 [§C] started...
; 5.005 [§A] woke after 5 seconds
; 5.005 [§C] acquired semaphore
; 5.005 [§A] released semaphore, exiting...
; 6.002 [§B] woke after 6 seconds
; 6.002 [§B] released semaphore, exiting...
; 9.010 [§C] woke after 4 seconds
; 9.010 [§C] released semaphore, exiting...
