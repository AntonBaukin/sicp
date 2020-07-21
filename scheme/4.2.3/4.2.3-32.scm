(define (log . args) (for-each display args) (newline))

(include "streams-lazy.scm")
(eval-basic (debug on))

;
; Sample 1. When we produce items via a mapping function,
; the values are not resolved at the map time.
;
(eval-basic
 (define items (list 1 2 3 4 5))
 (define scale (lambda (i) (* 2 i)))
 (define scaled (map scale items))

 ; We may alter the scale before the list items are memoized:
 (set! scale (lambda (i) (* 3 i)))

 ; Prints: (3 6 9 12 15) instead of expected 2-scale.
 (debug log "Sample 1. Delayed side-effect mapping: "
  (list->native scaled)
 )
)

;
; Sample 2. Items of stream produced when used.
;
(eval-basic
 (define c 0)

 ; As we turn off memoization for the function,
 ; we also turn it off up-way the resolves.
 (define (p) (memoff (set! c (+ c 1))))

 ; Here we create lazy stream with items produced
 ; with «p» — this is not so clear for Chapter 3
 ; streams as we have to use delayed items there.
 (define s (cons (p) s))

 (debug log "Sample 2. Producer with side-effect: " (slice 5 s))

 ; This sample prints: (1 2 3 4 5) stream slice.
 ; Note how we forced the same side-effect of «p»
 ; to repeat on resolving the same head item.
)

;
; Sample 3. Lets think of the next sample...
;
