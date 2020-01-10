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

; Hint: there is no need in separate op to multiply
; x0 and x1 — just else private variable to use.
(define (a-save-x)
 (set! x (* ax0 ax1))
 (log "A save x <— x0 * x1 = " x)
)

; Private variable of thread B:
(define bx void)

(define (b-read-x)
 (set! bx x)
 (log "B read x = " x)
)

(define (b-save-x)
 (set! x (+ bx 1))
 (log "B save x <— x + 1 = " x)
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
  (list b-read-x b-save-x)
 )
)

; Test AB demonstrates results of absent synchronization:
; values are: 11 100 101 110 121, — and only 101 and 121
; are valid in the case of full serialization.
(log "Test AB five results: " test-ab)


(define test-cd '())

; Private variables of thread C:
(define cxx void)

(define (c-read-x)
 (set! cxx (* x x))
 (log "C read x = " x "; calc x * x = " cxx)
)

(define (c-save-x)
 (log "C save x <— x * x = " cxx)
 (set! x cxx)
)

(define (d-save-x)
 (log "C read x = " x "; save x <— x + 1 = " (+ x 1))
 (set! x (+ x 1))
)

(define (done-cd)
 (set! test-cd (nums-add test-cd x))
 (log "Final x = " x)
 (log "——————————————————————————————————————————————————")
)

(test-shuffles
 reset
 done-cd
 (list
  (list c-read-x c-save-x)
  (list d-save-x)
 )
)

; Here we only see phenomenon of lost save (value 100):
(log "Test CD three results: " test-cd)
