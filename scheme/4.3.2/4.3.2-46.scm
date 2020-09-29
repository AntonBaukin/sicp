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
  "../4.3.2/4.3.2-46-amb.scm"
  "../4.3.1/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "../4.3.1/eval-amb.scm")
(include "4.3.2-45-parse.scm")

(eval-basic (debug on))

; With our reversed call order, we get reversed side-effect results:
(assert-equal? '(3 2 1)
 (eval-basic
  (define i 0)
  (define (inc) (set! i (+ i 1)))
  (define (abc a b c) (list a b c))
  (abc (inc) (inc) (inc))
 )
)

; And, as the task suggests, we parse the simplest test...
; That produces empty list...
;
(assert-eq? '() (eval-amb-results (parse '(the cat eats))))
;
; So, what it going wrong? Log trace demonstrates it:
;
; parse:> input = (the cat eats)
; parse:> word = «the» is verb?
;
; In «parse-sentence» it first tries to take a verb,
; then fails in require of «memq» in «parse-word».
;
; Also, the parser has side-effect on a global variable
; *unparsed*. This may cause further problems...
;
