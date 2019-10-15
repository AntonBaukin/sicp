
; Despite deque is related to a deque, we decided to
; completely separate the implementations not to make
; them dependent. But we create deque as two lists:
; first is the same as a deque, second is the back
; pointers having car to be reference to the pair
; of the forward list. Only o(1) pop() op requires
; backward list. We also add reverse iteration.
;
(define (make-deque-ops)
 (include "iterate.scm")

 (define null '())

 ; Two pairs of deque defines two lists:
 ; car — is forward first and last pointers;
 ; cdr — is the backward one.
 (define (make-empty)
  (cons (cons null null) (cons null null))
 )

 ; Traces the list to form a backward list:
 ; each car of this list refers pair of the
 ; forward one. Returns (b . w) pair, (car b)
 ; is is the last item of the forward list;
 ; (car w) refers the first one.
 (define (trace-backward lst)
  (define (next tail b w)
   (if (null? tail)
    (cons b w)
    (let ((n (cons tail b)))
     (next
      (cdr tail)
      n
      (if (null? w) n w)
     )
    )
   )
  )

  (next lst null null)
 )

 (define (make-from lst)
  (let ((bw (trace-backward lst)))
   (cons (cons (cadr bw) (caar bw)) bw)
  )
 )

 (define (empty? deque)
  (null? (caar deque))
 )

 (define (append deque item)
  (define f (cons item null))
  (define b (cons f null))

  (if (empty? deque)
   (begin
    (set-car! deque (cons f f))
    (set-cdr! deque (cons b b))
   )
   (begin
    ; We append items to the tail of the forward list:
    (set-cdr! (cdar deque) f)
    ; And retarget the last pair of it:
    (set-cdr! (car deque) f)

    ; We add first item of the backward list:
    (set-cdr! b (cadr deque))
    ; And retarget the first pair of it:
    (set-car! (cdr deque) b)
   )
  )
 )
 
 (define (check-empty deque)
  (if (empty? deque)
   (error "The deque is empty")
   void
  )
 )

 (define (take deque)
  (check-empty deque)
  (let ((first (caar deque)))
   (set-car! deque (cdr first))
   ; The deque became empty? Set the last pointer:
   (if (null? (cdr first)) (set-cdr! deque null) void)
   (car first) ;<— the value
  )
 )

 
 (list
  make-empty   ; 0
  make-from    ; 1
  empty?       ; 2
  append       ; 3
;  take         ; 4
;  first        ; 5
;  last         ; 6
;  push         ; 7
;  iterate      ; 8
;  qlength      ; 9
 )
)