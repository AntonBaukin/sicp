(include "../2.3.3/sorted-set.scm")
(include "combinations.scm")

(define (log . args) (for-each display args) (newline))

(define Numbers (make-sorted-set <))
(define nums-add (set-op-add Numbers))

; Shared variable:
(define x void)

(define (reset)
 (set! x 10)
 (log "\n\n" "Initial x = " x)
)


(define test-ab '())

; Private variables of thread A:
(define ax0 void)
(define ax1 void)

(define (a-read-x0)
 (set! ax0 x)
 (log "A read x0 = " x)
)

(define (a-read-x1)
 (set! ax1 x)
 (log "A read x1 = " x)
)

; See hint of task 39...
(define (a-save-x)
 (set! x (* ax0 ax1))
 (log "A save x <— x0 * x1 = " x)
)

; Private variables of thread B:
(define bx0 void)
(define bx1 void)
(define bx2 void)

(define (b-read-x0)
 (set! bx0 x)
 (log "B read x0 = " x)
)

(define (b-read-x1)
 (set! bx1 x)
 (log "B read x1 = " x)
)

(define (b-read-x2)
 (set! bx2 x)
 (log "B read x2 = " x)
)

(define (b-save-x)
 (set! x (* bx0 bx1 bx2))
 (log "B save x <— x0 * x1 * x2 = " x)
)

(define (done-ab)
 (set! test-ab (nums-add test-ab x))
 (log "Final x = " x)
 (log "——————————————————————————————————————————————————")
)

(test-shuffles
 reset
 done-ab
 (list
  (list a-read-x0 a-read-x1 a-save-x)
  (list b-read-x0 b-read-x1 b-read-x2 b-save-x)
 )
)

; Test AB demonstrates results of absent synchronization.
; The values are: 100 1000 10000 100000 1000000.
(log "Test AB results: " test-ab "\n\n")

; Fully serial variants are:
; 1) 10 * 10, 100 * 100 * 100  — gives 1000000;
; 2) 10 * 10 * 10, 1000 * 1000 — gives the same!

