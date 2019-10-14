
; Extends task 3.1 by making this function a high-order.
; It accepts the accumulating operation, and supports
; arbitrary number of arguments. To get current value,
; just call it without arguments.
;
(define (make-accumulator value op)
 (define (accumulator . args)
  (if (null? args) value
   ; Pass current value as the first argument:
   (let ((result (apply op (cons value args))))
    (set! value result)
    result
   )
  )
 )

 ; Gambit Scheme dislikes lambdas with just a var-args
 ; (arguments list starting with dot). We defined
 ; ordinary function and return it here:
 accumulator
)

; Applies given accumulator (actually, a visitor pattern)
; to each of the arguments following and returns it back.
(define (accumulate-each accumulator . args)
 (for-each accumulator args)
 accumulator ;<— allows to invoke it in a chain
)

; Creates string concatenating accumulator that uses given
; string separator and optional to-string formatter.
(define (make-concatenator sep . formatter)
 (define fmt
  (if
   (null? formatter)
   (lambda (s) s)
   (car formatter)
  )
 )

 (define (concat strings result)
  (if (null? strings) result
   (concat
    (cdr strings)
    (string-append
     result
     (if (= 0 (string-length result)) "" sep)
     (fmt (car strings))
    )
   )
  )
 )

 (make-accumulator ""
  (lambda (result . strings)
   (concat strings result)
  )
 )
)
