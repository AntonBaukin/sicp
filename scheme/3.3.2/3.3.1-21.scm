(include "queue.scm")
(include "assert.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

; Demonstrates combined power of iteration with
; visiting accumulator. This is a reduce().
(define (queue->string queue)
 (define S (make-concatenator " " symbol->string))
 (queue-iterate queue S)
 ; Call (S) returns accumulated string:
 (string-append "(" (S) ")")
)

; More show cases are in «queue-test.scm»
(define queue (queue-make-from '(a b c d e f)))
(log "Printing queue " queue " :> " (queue->string queue))

(define queue (queue-make))
(log "Empty queue: " (queue->string queue))
