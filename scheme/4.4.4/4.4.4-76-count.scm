
(define pattern-match-with-count
 (
  (lambda () ;<— immediately invoked function
   ; Save initial implementation of the matcher:
   (define pattern-match-impl pattern-match)

   ; This matcher increments global variable «count»:
   (define (pattern-match-count pattern assertion frame)
    (set! count (+ count 1))
    (pattern-match-impl pattern assertion frame)
   )

   (set! pattern-match pattern-match-count)
  )
 )
)
