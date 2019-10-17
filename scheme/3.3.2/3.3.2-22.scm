(include "iterate.scm")
(include "assert.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

; We implement a subset of «queue-ops.scm» operations.
; As we see, using scoped variables is much easier, and
; it allows us to involve any number of them without
; organizing them in a mesh of pairs.
;
(define (make-queue)
 (define null  '())
 (define first null)
 (define last  null)

 (define (empty?)
  (null? first)
 )

 (define (append item)
  (define p (cons item null))
  (if (empty?) (set! first p) (set-cdr! last p))
  (set! last p)
 )

 (define (check-empty)
  (if (empty?) (error "The queue is empty") void)
 )

 (define (pop)
  (check-empty)
  (let ((value (car first)))
   (set! first (cdr first))
   (if (null? first) (set! last null) void)
   value ;<— resulting value
  )
 )

 (define (iterate visitor)
  (iterate-list first visitor)
 )

 (lambda (method) ;<— resulting dispatcher
  (cond
   ((eq? method 'empty?)  empty?)
   ((eq? method 'append!) append)
   ((eq? method 'pop!)   pop)
   ((eq? method 'iterate) iterate)
   (else (error "Unknown queue method!" method))
  )
 )
)

(define (queue->string queue)
 (define S (make-concatenator " " symbol->string))
 ((queue 'iterate) S)
 ; Call (S) returns accumulated string:
 (string-append "(" (S) ")")
)

(define queue (make-queue))
(log "Empty queue: " (queue->string queue))

((queue 'append!) 'a)
(log "Single item: " (queue->string queue))

((queue 'append!) 'b)
(log "Two items: " (queue->string queue))

(for-each (queue 'append!) '(c d e f))
(log "More items: " (queue->string queue))

(assert-eq? 'a ((queue 'pop!)))
(log "Popped first: " (queue->string queue))

(assert-eq? 'b ((queue 'pop!)))
(log "Popped second: " (queue->string queue))

(assert-eq? 'c ((queue 'pop!)))
(log "Popped third: " (queue->string queue))
