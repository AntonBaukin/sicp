(include "2.5.3-rat-uni.scm")

; Use special version of rational package that applies
; unified operations as it's required in §2.5.3-2.93.
(define rational-package-init
 install-rational-uni-package
)

(include "2.5.3-base.scm")
(include "2.5.3-rat-cut-int.scm")
(include "2.5.3-polynomial-rat.scm")

(install-arithmetic-package
 'rational-cut-int
 install-rational-cut-int-package
)

(install-arithmetic-package
 'set-sparse-polynomial-cut
 (curry install-polynomial-cut-package polynomial-package)
)

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

(define (log-poly-rat-op ops op a b c d)
 (log
  "{" (num->str (make-rat a b)) "}"
  " " ops " "
  "{" (num->str (make-rat c d)) "}"
  " = "
  (num->str (op (make-rat a b) (make-rat c d)))
 )
)
