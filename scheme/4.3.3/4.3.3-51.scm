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
  "../4.3.3/4.3.3-51-pset.scm"
  "../4.3.1/eval-impl-set.scm"
 )
)

(include "../4.3.1/eval-amb.scm")

(log "—— Permanent set counting:" "\n"
 (eval-amb-results
  (define count 0)

  (let (
    (x (amb 'a 'b 'c))
    (y (amb 'a 'b 'c))
   )
   (permanent-set! count (+ count 1))
   (require (not (eq? x y)))
   (list x y count)
  )
 )
)

; Here are the results expected:
; >> ((a b 2) (a c 3) (b a 4) (b c 6) (c a 7) (c b 8))

(log "—— Default amb set counting:" "\n"
 (eval-amb-results
  (define count 0)

  (let (
    (x (amb 'a 'b 'c))
    (y (amb 'a 'b 'c))
   )
   (set! count (+ count 1))
   (require (not (eq? x y)))
   (list x y count)
  )
 )
)

; Counter is always 1. In task «4.3.1/4.3.1-37.scm» we've used
; special form debug for the same purpose: (debug inc counter).
; >> ((a b 1) (a c 1) (b a 1) (b c 1) (c a 1) (c b 1))
