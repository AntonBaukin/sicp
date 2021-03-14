;
; Procedures of QEval: and, or, not, lisp-value.
; Depends on: «stream.scm», «defs.scm», and «utilities.scm».
;

(define (qproc-and conjuncts frame-stream)
 (if (null? conjuncts)
  frame-stream
  (qproc-and
   (cdr conjuncts)
   (qeval-disp
    (make-pattern (car conjuncts))
    frame-stream
   )
  )
 )
)

(define (qproc-or disjuncts frame-stream)
 (if (null? disjuncts)
  the-empty-stream
  (interleave-delayed
   (qeval-disp
    (make-pattern (car disjuncts))
    frame-stream
   )
   (delay
    (qproc-or (cdr disjuncts) frame-stream)
   )
  )
 )
)

(define (qproc-not operands frame-stream)
 (stream-flatmap
  (lambda (frame)
   (define single-frame (singleton-stream frame))
   (define negated-frames
    (qeval-disp
     (make-pattern (car operands))
     single-frame
    )
   )

   (if (stream-null? negated-frames)
    (singleton-stream frame)
    the-empty-stream
   )
  )
  frame-stream
 )
)

(define (lisp-value-match? call frame)
 (eval (prepare-for-eval (instantiate call frame)))
)

(define (qproc-lisp-value call frame-stream)
 (stream-flatmap
  (lambda (frame)
   (if (lisp-value-match? call frame)
    (singleton-stream frame)
    the-empty-stream
   )
  )
  frame-stream
 )
)

(define (qproc-always-true ignore frame-stream)
 frame-stream
)

; Allows to add variable to a frame in to variants:
;
; 1) (set ?x exp) where «exp» may be any plain value or
;    list composition with else variables.
;
; 2) (set ?x fname args...) where «fname» is a name of
;    global function taking some arguments, same as
;    «lisp-value» special rule takes.
;
; This form allows us to build new values that may not
; be deduced, such as in recursive samples of SICP,
; that cause infinite progression.
;
(define (qproc-set exp frame-stream)
 (define (set-value frame exp)
  (frame-bind
   frame
   (variable-name (car exp))
   (cadr exp)
  )
 )

 (define (set-eval frame exp)
  (frame-bind
   frame
   (variable-name (car exp))
   (eval (prepare-for-eval (instantiate (cdr exp) frame)))
  )
 )

 (cond
  ((and (= (length exp) 2) (variable? (car exp)))
   (stream-flatmap
    (lambda (frame)
     (singleton-stream (set-value frame exp))
    )
    frame-stream
   )
  )
  ((and (> (length exp) 2) (variable? (car exp)))
   (stream-flatmap
    (lambda (frame)
     (singleton-stream (set-eval frame exp))
    )
    frame-stream
   )
  )
  (else (error "Wrong «set» form" exp))
 )
)

; Simple «amb» form. The syntax is following:
; (amb ?x value) where value may be a plain list,
; a variable, or a list of variables
;
; Produce function takes (value) and returns the next
; value, or empty list to stop.
;
; Form produces a stream of frames where «?x» takes
; each value from the list.
;
(define (qproc-amb exp frame-stream)
 (define (amb-any var value frame)
  (cond
   ((variable? value)
    (let ((b (frame-get frame (variable-name value))))
     (if (null? b)
      (singleton-stream (frame-bind frame var value))
      (amb-any var (binding-value b) frame)
     )
    )
   )

   ((list? value)
    (stream-flatmap
     (lambda (item)
      (amb-any var item frame)
     )
     (list->stream value)
    )
   )

   ((pair? value)
    (amb-any var (list (car value) (cdr value)) frame)
   )

   (else
    (singleton-stream (frame-bind frame var value))
   )
  )
 )

 (if (and (= (length exp) 2) (variable? (car exp)))
  (stream-flatmap
   (lambda (frame)
    (amb-any (variable-name (car exp)) (cadr exp) frame)
   )
   frame-stream
  )
  (error "Wrong «amb» form" exp)
 )
)

; Override this mapping list with your own implementations.
(define set-qeval-procs
 (
  (lambda () ;<— immediately invoked function
   (set! qeval-procs
    (list
     (list 'and qproc-and)
     (list 'or  qproc-or)
     (list 'not qproc-not)
     (list 'lisp-value qproc-lisp-value)
     (list 'always-true qproc-always-true)
     (list 'set qproc-set)
     (list 'amb qproc-amb)
    )
   )
  )
 )
)
