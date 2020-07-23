(define (log . args) (for-each display args) (newline))

(include "streams-lazy.scm")
(eval-basic (debug on))

;
; As we see by the following samples, the key difference
; between lazy streams and cdr-delayed streams of Chapter 3,
; is that the lazy car-part is calculated on demand, thus we
; are able to place side-effects here.
;
; Side-effects are not so bad in functional programming
; when they are located in local scope of a function.
; (Not talking about the input-output.)
;
; If we use only pure functions in the car-operation of a stream,
; the is no difference between two implementation variants, and
; only side-effects do play the role for lazy streams.
;

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
; Sample 3. With delayed side-effect, we can create stream
; mapping function in iterative way without recursion on
; delayed cdr-part of the stream.
;
; Note that in our evaluators we have no tail recursion
; optimization, and iterative mapping may be the only
; effective way of processing streams!
;
(eval-basic
 (define (inc i) (+ i 1))

 ; A stream of integers with lazy streams is created
 ; in the same way as with delayed streams:
 (define ints (cons 1 (map inc ints)))

 (define (imap proc s)
  (define (next) (let ((x (car s))) (set! s (cdr s)) (proc x)))
  (define r (cons (memoff (next)) r)) ;<— the call result
 )

 (define squares (imap square ints))

 (debug log "Sample 3. Iterative mapping function: " (slice 5 squares))
)
