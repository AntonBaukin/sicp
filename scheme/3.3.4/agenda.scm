(include "../3.3.2/queue.scm")
(include "../3.3.3/tree-red-black.scm")
(include "../2.5.3/index-tree.scm")


; Creates Agenda as incapsulated object of SICP §3.3.4.
; The result of the call is the list of instance ops.
; They are available via call to agenda-param.
;
; Required parameters of the list:
;  0 inverter-delay
;  1 and-gate-delay
;  2 or-gate-delay
;
; Optional parameters (default: #t):
;  3 immediate, required for task 3.31
;  4 queue, required for task 3.32.
;
; Agenda incapsulates wire implementation via make-wire.
; Wire is list of: ('wire agenda signal ...),
; where signal is #f or #t.
;
; Immediate is a wire parameter that tells to invoke
; on-wire callback when attaching to that wire.
;
; Queue parameter tells to store segment actions as
; a FIFO queue, or as a LIFO-stack (see task 3.32).
;
(define (make-agenda params)
 (define time 0)

 ; Maps (time —> actions queue).
 (define segments (make-index-tree))
 (define get-segment (index-tree-get segments))
 (define add-segment (index-tree-set segments))
 (define del-segment (index-tree-del segments))
 (define segments-iter (index-tree-iter segments))

 (define (param name)
  (cond
   ((eq? name 'inverter-delay)
    (list-ref params 0)
   )

   ((eq? name 'and-gate-delay)
    (list-ref params 1)
   )

   ((eq? name 'or-gate-delay)
    (list-ref params 2)
   )

   ((eq? name 'immediate)
    (if (> (length params) 3) (list-ref params 3) #t)
   )

   ((eq? name 'queue)
    (if (> (length params) 4) (list-ref params 4) #t)
   )

   (else void)
  )
 )

 (define (make-wire)
  (list 'wire agenda #f (queue-make))
 )

 (define (call-wire wire)
  (queue-iterate (list-ref wire 3) (lambda (a) (a wire)))
 )

 (define (get-time) time)

 (define (check-delay delay)
  (if (and (number? delay) (>= delay 0)) delay
   (error "Not a valid delay number value!" delay)
  )
 )

 (define (segment-queue t)
  (define s (get-segment t))

  (if (not (null? s)) s
   (let ((s (queue-make)))
    (add-segment t s)
    s
   )
  )
 )

 (define (after-delay delay action)
  (define queue? (param 'queue))
  (define q (segment-queue (+ time (check-delay delay))))

  (if (procedure? action)
   (if queue?
    (queue-append! q action)
    (queue-push! q action)
   )

   (error "Register not a function on delayed agenda action!" action)
  )
 )

 ; Returns (time . queue) of the first segment, or void.
 (define (first-segment)
  (segments-iter (lambda (time queue) (cons time queue)))
 )

 (define (propagate)
  (define tq (first-segment))

  (if (pair? tq)
   (begin
    (if (< (car tq) time)
     (error "Agenda segmentation time fault!" time)
    )

    ; Set current time and remove the segment:
    (set! time (car tq))
    (del-segment (car tq))

    ; Iterate and call all actions of the segment:
    (queue-iterate (cdr tq) (lambda (a) (a agenda)))

    (propagate) ;<— recurse the invocation
   )
  )
 )

 (define agenda
  (list
   'agenda     ; 0
   param       ; 1
   make-wire   ; 2
   call-wire   ; 3
   get-time    ; 4
   after-delay ; 5
   propagate   ; 6
  )
 )

 agenda ;<— resulting ops is agenda itself
)

; Agenda is always 1th of the item's list.
; Call has two forms: (item), (type item).
; Type is a symbol to check, or void.
(define (agenda-bound? . args)
 (define type (if (= 1 (length args)) void (car args)))
 (define item (if (= 1 (length args)) (car args) (cadr args)))

 (and
  (pair? item)
  (pair? (cdr item))
  (or
   (eq? void type)
   (eq? type (car item))
  )
  (pair? (cadr item))
  (eq? 'agenda (caadr item))
 )
)

; Returns agenda of the given item: a wire, any element.
(define (get-agenda item)
 (cond
  ((agenda-bound? item) (cadr item))
  ((and (pair? item) (eq? 'agenda (car item))) item)
  (else (error "Not an agenda bound item!" item))
 )
)

(define (agenda-param item name)
 ((list-ref (get-agenda item) 1) name)
)

; Returns current time of agenda or it's item.
(define (get-time item)
 ((list-ref (get-agenda item) 4))
)

; Returns new wire instance bound to this agenda.
(define (make-wire item)
 ((list-ref (get-agenda item) 2))
)

; Returns boolean signal value: #t or #f.
(define (get-signal wire)
 (if (agenda-bound? 'wire wire)
  (list-ref wire 2)
  (error "Get signal of not a wire!" wire)
 )
)

; Assigns signal value to the given wire and
; returns #t if signal was actually changed.
(define (set-signal wire s)
 (if (not (agenda-bound? 'wire wire))
  (error "Set signal of not a wire!" wire)
 )

 (if (not (or (eq? #t s) (eq? #f s)))
  (error "Not a valid signal value!" s)
 )

 (if (not (eq? s (list-ref wire 2)))
  (begin ; Set the value and invoke wire actions:
   (set-car! (cddr wire) s)
   ((list-ref (cadr wire) 3) wire)
  )
 )

 wire
)

; Adds action procedure that listens on wire
; signal change. Action takes: (wire) argument.
(define (on-wire wire action)
 (if (not (agenda-bound? 'wire wire))
  (error "Register action on not a wire!" wire)
 )

 (if (not (procedure? action))
  (error "Register not a function on wire action!" action)
 )

 ; Add action to the wire's queue:
 (queue-append! (list-ref wire 3) action)

 ; Run action now? — see task SICP 3.31 in §3.3.4:
 (let ((i (agenda-param wire 'immediate)))
  (if i (action wire))
 )

 wire
)

; Register given action function to be invoked after
; the delay in the agenda or agenda boubd item.
; Action arguments: (agenda).
(define (after-delay item delay action)
 ((list-ref (get-agenda item) 5) delay action)
)

(define (propagate agenda)
 ((list-ref (get-agenda agenda) 6))
)
