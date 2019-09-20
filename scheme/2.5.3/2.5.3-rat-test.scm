(include "2.5.3-rat-base.scm")

(define (log-rat n d)
 (let ((r (make-rat n d)))
  (log r " = " (num->str r))
 )
)

(define (log-rat-op ops op a b c d)
 (log
  ; Display original number values:
  (num->str a) "/" (num->str b)
  " " ops " "
  (num->str c) "/" (num->str d)
  " = "
  (num->str (op (make-rat a b) (make-rat c d)))
 )
)

(log-rat (I 1) (I 3))
(log-rat (N 2) (N 4))

(log-rat-op "+" add (I 1) (I 3) (N 1) (N 3))
(log-rat-op "+" add (I 2) (I 5) (I 1) (I 3))
(log-rat-op "*" mul (I 1) (I 3) (N 1) (N 3))
(log-rat-op "*" mul (I 1) (I 2) (I 2) (I 3))
(log-rat-op "-" sub (I 1) (I 2) (I 1) (I 3))
(log-rat-op "/" div (I 1) (I 2) (I 1) (I 3))
