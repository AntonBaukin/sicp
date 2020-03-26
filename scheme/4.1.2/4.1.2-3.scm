(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

; For dispatching evaluator, see «eval-impl-disp.scm».
; Special forms are defined in «eval-impl-forms.scm».

(assert-eq? 24
 (eval-basic
   (define (factorial n)
    ; Cool printing of the stack frame:
    (debug log-stack "\n—— factorial " n " ——")
    (if (= n 1) 1 (* n (factorial (- n 1))))
   )
   (factorial 4)
 )
)

; —— factorial 4 ——
; ~> Frame [0 of 1]
;    n .... 4
;
; —— factorial 3 ——
; ~> Frame [0 of 2]
;    n .... 3
; ~> Frame [1 of 2]
;    n .... 4
;
; —— factorial 2 ——
; ~> Frame [0 of 3]
;    n .... 2
; ~> Frame [1 of 3]
;    n .... 3
; ~> Frame [2 of 3]
;    n .... 4
;
; —— factorial 1 ——
; ~> Frame [0 of 4]
;    n .... 1
; ~> Frame [1 of 4]
;    n .... 2
; ~> Frame [2 of 4]
;    n .... 3
; ~> Frame [3 of 4]
;    n .... 4
