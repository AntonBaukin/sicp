(include "tree-test-base.scm")
(include "../3.1/random.scm")
(include "../3.1/enumerate.scm")


; Average tree size of the random tests: 0.5*N.
(define-value-if-not 'N 20)

; Use random seed and report it to console.
(define-value-if-not 'seed (exact (truncate (current-second))))
(define random (make-random seed))

; Does reset the random generator depending on the argument:
;  – without argument, resets to the initial seed;
;  – 'time symbol, takes new seed by current time millisecond;
;  – else treats the value as the new seed.
;
; Running reset makes sequential tests independent.
; This simplifies debugging
;
(define (random-reset . arg)
 (cond
  ((null? arg) (random 'reset))

  ((eq? 'time (car arg))
   (set! seed (exact (truncate (* 1000 (current-second)))))
   (random seed)
  )

  (else
   (set! seed (car arg))
   (random seed)
  )
 )
)

; Random [1 N] selector.
(define random-N (make-random-in-range random 1 (+ N 1)))

; Random numbers of the tree in [0 .. 2N) range.
; Warning! Numbers range must be larger than N
; because we generate unique numbers.
(define random-num (make-random-in-range random 0 (* 2 N)))

; This index is assigned on each test:
(define global-test-index -1)

; Assign this to failed index to make ilog() working.
(define log-index -1)

; This special trace logger is intended to trace
; 
(define (ilog . args)
 (if (eq? global-test-index log-index)
  (apply log args) void
 )
)

(define (run-test-guard index test)
 (set! global-test-index index)

 (if (eq? global-test-index log-index)
  ; Run target test directly to see the error details:
  (test index)

  ; Run test guarded:
  (let ((result
    (with-exception-catcher
     (lambda (e) e)
     (lambda () (test index) void)
    )
   ))

   (if (eq? void result) void
    (begin
     (log "Break test on error @index: " index)
     (raise result)
    )
   )
  )
 )
)

(define (run-test-gen test index)
 ; Generate test tree size:
 (define n (assert-test (random-N) (list > - 0)))

 ; Generate n unique numbers to form the tree:
 (define source (assert-test
  (produce-n-unique n eq? random-num)
  (list = n length) ;<— predicate (= n (length source))
 ))

 (run-test-guard index
  (lambda (index) (test index n source))
 )
)

(define (run-test-gen-cycles T test)
 (map (curry run-test-gen test) (enumerate-n T))
 (log "Successfully completed tests: " T)
)
