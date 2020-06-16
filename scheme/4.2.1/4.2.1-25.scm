(define (log . args) (for-each display args) (newline))

(include "../4.1.7/eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

(eval-basic
 (define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value)
 )

 (define (factorial n)
  (debug log-env "—— factorial [" n "] ——")
  (debug pause)
  (unless (= 1 n) (* n (factorial (- n 1))) 1)
 )

 (factorial 5)
)

; As expected, this call never ends as «usual-value»
; is evaluated before checking the condition.
;
; With the help of or debug mode we may see the stack:
;
; —— factorial [-1] ——
;
; > Env #2 env-uid-4 factorial (n)
; ~> Frame [0 of 7]
;    n .... -1
; ~> Frame [1 of 7]
;    n .... 0
; ~> Frame [2 of 7]
;    n .... 1
; ~> Frame [3 of 7]
;    n .... 2
; ~> Frame [4 of 7]
;    n .... 3
; ~> Frame [5 of 7]
;    n .... 4
; ~> Frame [6 of 7]
;    n .... 5
