
(eval-basic
 (define (std-map-with with streams res)
  (if (null? streams)
   (reverse res)
   (std-map-with
    with
    (cdr streams)
    (cons (with streams) res)
   )
  )
 )

 (define (std-map-next mapper streams res)
  (if (null? (car streams))
   (reverse res)
   (std-map-next
    mapper
    (std-map-with cdar streams '())
    (cons
     (apply mapper (std-map-with caar streams '()))
     res
    )
   )
  )
 )

 (define (std-map mapper . streams)
  (std-map-next mapper streams '())
 )

 ; Define global map function:
 (global map std-map)
)
