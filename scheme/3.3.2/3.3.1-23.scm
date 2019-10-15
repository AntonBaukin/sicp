(include "deque.scm")
(include "assert.scm")
(include "../2.4.3/curry.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define deque (deque-make-from '(a b c)))

(assert-eq? 'a (deque-first deque))
(assert-eq? 'c (deque-last deque))
(assert-eq? 'a (deque-pop! deque))
(assert-eq? 'b (deque-first deque))
(assert-eq? 'b (deque-pop! deque))
(assert-eq? 'c (deque-first deque))
(assert-eq? 'c (deque-last deque))
(assert-eq? 'c (deque-pop! deque))
(assert-true? (deque-empty? deque))


(define (deque->string deque)
 (define S (make-concatenator " " symbol->string))
 (deque-iterate deque S)
 ; Call (S) returns accumulated string:
 (string-append "(" (S) ")")
)

(define deque (deque-make))
(log "Empty deque: " (deque->string deque))

(deque-append! deque 'a)
(assert-eq? 'a (deque-first deque))
(assert-eq? 'a (deque-last deque))
(log "Single item: " (deque->string deque))

(deque-append! deque 'b)
(assert-eq? 'a (deque-first deque))
(assert-eq? 'b (deque-last deque))
(log "Two items: " (deque->string deque))

(for-each (curry deque-append! deque) '(c d e f))
(assert-eq? 'a (deque-first deque))
(assert-eq? 'f (deque-last deque))
(log "More items: " (deque->string deque))

(assert-eq? 'a (deque-pop! deque))
(assert-eq? 'b (deque-first deque))
(assert-eq? 'f (deque-last deque))
(log "Popped first: " (deque->string deque))

(assert-eq? 'b (deque-pop! deque))
(assert-eq? 'c (deque-first deque))
(assert-eq? 'f (deque-last deque))
(log "Popped second: " (deque->string deque))

(deque-push! deque 'b)
(assert-eq? 'b (deque-pop! deque))
(deque-push! deque 'b)
(deque-push! deque 'a)
(assert-eq? 'a (deque-first deque))
(assert-eq? 'f (deque-last deque))
(log "Returned them back: " (deque->string deque))
(assert-eq? 6 (deque-length deque))

(assert-eq? 'f (deque-take! deque))
(assert-eq? 'a (deque-first deque))
(assert-eq? 'e (deque-last deque))
(assert-eq? 5 (deque-length deque))
(log "Taken last: " (deque->string deque))

(deque-append! deque 'f)
(assert-eq? 'a (deque-first deque))
(assert-eq? 'f (deque-last deque))
(assert-eq? 6 (deque-length deque))
(log "Appended it back: " (deque->string deque))
