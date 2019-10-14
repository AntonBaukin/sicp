(include "queue.scm")
(include "assert.scm")
(include "../2.4.3/curry.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))


(define (queue->string queue)
 (define S (make-concatenator " " symbol->string))
 (queue-iterate queue S)
 ; Call (S) returns accumulated string:
 (string-append "(" (S) ")")
)

(define queue (queue-make))
(log "Empty queue: " (queue->string queue))

(queue-append! queue 'a)
(assert-eq? 'a (queue-first queue))
(assert-eq? 'a (queue-last queue))
(log "Single item: " (queue->string queue))

(queue-append! queue 'b)
(assert-eq? 'a (queue-first queue))
(assert-eq? 'b (queue-last queue))
(log "Two items: " (queue->string queue))

(for-each (curry queue-append! queue) '(c d e f))
(assert-eq? 'a (queue-first queue))
(assert-eq? 'f (queue-last queue))
(log "More items: " (queue->string queue))

(assert-eq? 'a (queue-take! queue))
(assert-eq? 'b (queue-first queue))
(assert-eq? 'f (queue-last queue))
(log "Took first: " (queue->string queue))

(assert-eq? 'b (queue-take! queue))
(assert-eq? 'c (queue-first queue))
(assert-eq? 'f (queue-last queue))
(log "Took second: " (queue->string queue))

(queue-push! queue 'b)
(queue-push! queue 'a)
(assert-eq? 'a (queue-first queue))
(assert-eq? 'f (queue-last queue))
(log "Returned them back: " (queue->string queue))
(log "Queue length: " (queue-length queue))
