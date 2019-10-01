(include "2.1.4-2.y.scm")

(define (interval-mul-approx i j)
 (interval-make-%
  (*
   (interval-center i)
   (interval-center j)
  )

  (+
   (interval-% i)
   (interval-% j)
  )
 )
)

(log "[5 ± 2%] * [3 ± 5%] == "
 (interval-str (interval-mul
  (interval-make-% 5 2)
  (interval-make-% 3 5)
 ))
)

(log "[5 ± 2%] * [3 ± 5%] ~= "
 (interval-str (interval-mul-approx
  (interval-make-% 5 2)
  (interval-make-% 3 5)
 ))
)

(log "[5 ± 2%] * [3 ± 5%] == "
 (interval-str-% (interval-mul
  (interval-make-% 5 2)
  (interval-make-% 3 5)
 ))
)

(log "[5 ± 2%] * [3 ± 5%] ~= "
 (interval-str-% (interval-mul-approx
  (interval-make-% 5 2)
  (interval-make-% 3 5)
 ))
)
