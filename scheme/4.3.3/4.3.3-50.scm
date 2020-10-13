(define (log . args) (for-each display args) (newline))

(define eval-amb-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.7/eval-impl-registry.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.3.1/eval-impl-amb.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.3.1/eval-impl-forms.scm"
  "../4.3.3/4.3.3-50-ramb.scm"
  "../4.3.1/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "../4.3.1/eval-amb.scm")
(include "../4.3.2/4.3.2-45-parse.scm")
(include "../4.3.2/4.3.2-48-parse.scm")
(include "../4.3.2/4.3.2-49-gen.scm")
(include "4.3.3-50-gen.scm")
(include "4.3.3-50-test.scm")

; Turn on debugging to trace the generation:
;(eval-basic (debug on))

; Test how «ramb» selects from 1..5 with the given seed:
(eval-basic (ramb-seed 1000037))
(test 3 5 3 3 5 2 4 5)

(define (spaces phrase)
 (append
  (list (car phrase))
  (if (null? (cdr phrase)) '() (append '(" ") (spaces (cdr phrase))))
 )
)

(eval-basic (ramb-seed 100003))

(for-each
 (lambda (sentence)
  (apply log (append '(">> ") (spaces sentence) '("\n")))
 )
 (eval-amb-lim 10 (generate))
)
;
; The results following are more variable, but still,
; the initial branch of the recursion «a cat calmly
; studies» is sticky.
;
; After 10000 items, it's still a cat...
;
; >> a cat calmly studies
;
; >> a cat calmly studies calmly
;
; >> a cat calmly studies hastily
;
; >> a cat calmly studies on a lazy student
;
; >> a cat calmly studies on a lazy floor
;
; >> a cat calmly studies on a lazy class
;
; >> a cat calmly studies on a lazy cat
;
; >> a cat calmly studies on a lazy desk
;
; >> a cat calmly studies on a lazy professor
;
; >> a cat calmly studies on a white professor
;
; ... skipped 9990 ...
;
; >> a cat for the white student studies
;
; >> a cat for the white student studies calmly
;
; >> a cat for the white student studies hastily
;
; >> a cat for the white student sleeps hastily
;
; >> a cat for the white student sleeps calmly
;
; >> a cat for the white student sleeps
;
; >> a cat for the white student calmly lectures
;
; >> a cat for the white student calmly eats
;
; >> a cat for the white student calmly sleeps
;
; >> a cat for the white student calmly studies
