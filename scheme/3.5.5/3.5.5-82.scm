(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.5.3/signal.scm")
(include "../3.5.3/float-str.scm")
(include "random-stream.scm")

(define seed 1000000007)

(define (log . args) (for-each display args) (newline))

(define (estimate-integral x0 x1 y0 y1 random-stream predicate)
 (define area (abs (* (- x0 x1) (- y0 y1))))

 (define random-x
  (make-random-stream-in-range x0 x1
   ; We sample even random items for X:
   (stream-nth 2 random-stream)
  )
 )

 (define random-y
  (make-random-stream-in-range y0 y1
   ; We sample odd random items for Y:
   (stream-nth 2 (stream-cdr random-stream))
  )
 )

 ; Takes random X and Y in the range, and (failed . passed)
 ; pair. Returns new pair based on the experiment.
 (define (test x y fp)
  (if (predicate x y) ; <— passed?
   (cons (car fp) (+ 1 (cdr fp)))
   (cons (+ 1 (car fp)) (cdr fp))
  )
 )

 (define (estimate fp)
  (/ (* area (cdr fp)) (+ (car fp) (cdr fp)))
 )

 (define trials
  (cons-stream '(0 . 0)
   (stream-map test random-x random-y trials)
  )
 )

 (stream-map estimate (stream-cdr trials))
)

(define est-parabola-segmnent
 (estimate-integral
  0.0 1.0 0.0 1.0
  (make-random-stream seed)
  (lambda (x y) (<= y (square x)))
 )
)

(define (iv->str iv)
 (string-append
  "[" (number->string (car iv))
  "]\t" (float->str -5 (cdr iv))
 )
)

(log "Area of parabola y = x² segment on [0; 1]:")
(map log
 (map iv->str
  (cdr 
   (sub-stream->list 10
    (sample-signal est-parabola-segmnent 10000)
   )
  )
 )
)

; As we see, this estimation is very coarse.
; This is due granularity of our random generetor.
;
; [10000] +0.32916
; [20000] +0.33173
; [30000] +0.33152
; [40000] +0.33106
; [50000] +0.33253
; [60000] +0.33297
; [70000] +0.3343
; [80000] +0.3348
; [90000] +0.33517
