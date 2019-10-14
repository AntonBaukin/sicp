(define (log . args) (for-each display args) (newline))

; The core of o(1) memory loop detection.
; Marker function takes the list' head item
; and checks whether it's already marked.
;
; It's statefull: it counts the expected
; loop length to expect to meet the marker
; again. When the await-length comes zero,
; doubles it and marks again.
;
; This simple technique solves the problem
; that we do not know whether the loop exists,
; and what is the length of the loop.
;
(define (make-marker)
 ; Current marker:
 (define marker void) ;<— current marker
 (define length 1)    ;<— expected length of the loop
 (define left   0)    ;<— current length to wait

 (lambda (item)
  (cond
   ; Got to the marker: yes, looped:
   ((eq? item marker) #t)

   ; Exhausted the expected length
   ((= 0 left)
    (set! marker item) ;<— mark again
    (set! length (* length 2)) ;<— double the length
    (set! left length) ;<— await doubled length
    #f
   )

   (else
    (set! left (- left 1))
    #f
   )
  )

 )
)

(define (looped? lst)
 (define marked? (make-marker))

 (define (iter tail)
  (cond
   ((null? tail) #f)
   ((marked? tail) #t) ;<— mark current pair
   (else (iter (cdr tail)))
  )
 )

 (iter lst)
)

(define (enumerate-range a b)
 (define (iter i res)
  (if (< i a) res (iter (- i 1) (cons i res)))
 )

 (iter b (list))
)

(define (make-list n)
 (enumerate-range 0 (- n 1))
)

(define (test-not-looped n)
 (if (looped? (make-list n))
  (error "False looped list" n)
  void
 )
)

(define (last-pair l)
 (if (null? (cdr l)) l
  (last-pair (cdr l))
 )
)

(define (make-looped-list n)
 (let ((l (make-list n)))
  (set-cdr! (last-pair l) l)
  l
 )
)

(define (test-looped n)
 (if (looped? (make-looped-list n))
  void
  (error "False not looped list" n)
 )
)

(for-each test-not-looped (enumerate-range 1 1000))
(log "successfully tested 1 - 1000 not looped lists")

(for-each test-looped (enumerate-range 1 1000))
(log "successfully tested 1 - 1000 looped lists")
