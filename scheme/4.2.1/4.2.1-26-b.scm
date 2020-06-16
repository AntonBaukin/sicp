(include "../3.3.2/assert.scm")

(define (log . args) (for-each display args) (newline))

; Reduce function.
(define (reduce zero join sequence)
 (define (iter res tail)
  (if (null? tail) res
   (iter (join res (car tail)) (cdr tail))
  )
 )

 (iter zero sequence)
)

; Classical HOF that binds leading arguments.
(define (curry f . xargs)
 (define (curried . args)
  (apply f (append xargs args))
 )

 curried ;<— resulting function
)

; Sample usage of «curry»:
(define summer (curry reduce 0 +))
(assert-eq? 10 (summer '(1 2 3 4)))

; HOF «tester» takes a predicate. Resulting function
; applies predicate to the given args and «conses»
; p-boolean-result with that args.
(define (tester p)
 (define (tester-p . args)
  (cons (apply p args) args)
 )

 tester-p ;<— resulting function
)

; HOF «spread» composes two functions «f» and «g».
; It first invokes «g» that returns a list, then
; applies «f» with that list.
(define (spread f g)
 (define (f∘g . args)
  (define x (apply g args))
  (apply f (if (list? x) x (list x)))
 )

 f∘g ;<— resulting composition
)

; So, our replacements for «unless» form:
(define (UNLESS test a b) (if test b a))

; And now, comes some HOF magic...
(define magic (curry reduce 0 (spread UNLESS (tester <))))

; As you may guess, it returns maximum positive value...
(assert-eq? 10 (magic '(1 4 9 3 6 10 5 2 8 7)))
(assert-eq? 0 (magic '(-1 -10)))
