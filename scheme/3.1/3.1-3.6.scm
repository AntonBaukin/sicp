(include "enumerate.scm")
(include "random.scm")

(define (log . args) (for-each display args) (newline))

(define (make-random seed)
 (define a 25214903917)
 (define c 11)
 (define m 281474976710656)
 (define s seed)


 (define (next)
  (set! seed (modulo (+ c (* seed a)) m))
  (modulo
   (truncate-quotient seed 65536) ; <— shift 16 bits
   4294967296 ; <— and with 32 bits
  )
 )

 (define (reset)
  (set! seed s)
 )

 (define (cmd? args name)
  (and
   (= 1 (length args))
   (eq? name (car args))
  )
 )

 (define (exec-cmd args)
  (if (cmd? args 'reset) (reset)
   (else "Unknown rand command" args)
  )
 )

 ; Despite task 3.6 asks us to generate on 'generate
 ; command, this makes the interface of random differ
 ; from common — has no arguments, — but we follow it.
 (define (rand . args)
  (if (null? args) (next) (exec-cmd args))
 )

 rand
)


(define random (make-random 1))
(define random-byte (make-random-in-range random 0 255))
(define range (enumerate-range 0 9))

(define msg-0 (map (lambda (i) (random-byte)) range))
(log "10 random bytes: " msg-0)

(random 'reset)
(define msg-1 (map (lambda (i) (random-byte)) range))
(log "and them again : " msg-1)
