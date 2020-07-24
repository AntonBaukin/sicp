(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.7/eval-impl-registry.scm"
  "../4.2.2/eval-impl-thunk.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.7/eval-impl-analyze.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.2.2/eval-impl-lazy.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.7/eval-impl-forms.scm"
  "../4.2.2/eval-impl-lazy-forms.scm"
  ; Here we support debug printing of new lists:
  "../4.2.3/4.2.3-34-debug.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "streams-lazy.scm")
(eval-basic (debug on))

(eval-basic
 (debug log "Debug log of simple values: " "ABC" " " 1 " " 'A)

 ; As task 33 is not involved here, quoted lists are still native:
 (debug log "Debug log of a native list: " '(a b c))

 (define lazy-list (list a b c))
 (define a 1) (define b 2) (define c 3)

 ; Here we print a lazy list:
 (debug log "Debug log of a lazy list: " lazy-list)

 (define (inc i) (+ i 1))
 (define ints (cons 1 (map inc ints)))

 (debug log "Debug log of an integers stream: " ints)
)
