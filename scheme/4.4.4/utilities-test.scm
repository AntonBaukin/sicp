(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "defs.scm")
(include "utilities.scm")

; Query parser: plain assertions.
(assert-equal? '(boy Mike likes girl Alice)
 (parse-query '(boy Mike likes girl Alice))
)

(assert-equal? '(boy Mike likes girl Alice)
 (print-query '(boy Mike likes girl Alice))
)

; Query parser: leading variable.
(assert-equal? '((? . gender) Mike)
 (parse-query '(?gender Mike))
)

(assert-equal? '(?gender Mike)
 (print-query '((? . gender) Mike))
)

; Query parser: rule with variables.
(assert-equal?
 '(rule
   (likes (? . boy) (? . girl))
   (and
    (boy (? . boy))
    (girl (? . girl))
    (boy (? . boy) likes girl (? . girl))
   )
  )
 (parse-query
  '(rule (likes ?boy ?girl)
    (and
     (boy ?boy)
     (girl ?girl)
     (boy ?boy likes girl ?girl)
    )
   )
 )
)

(assert-equal?
 '(rule (likes ?boy ?girl) (and (boy ?boy) (girl ?girl) (boy ?boy likes girl ?girl)))
 (print-query
  '(rule
    (likes (? . boy) (? . girl))
    (and
     (boy (? . boy))
     (girl (? . girl))
     (boy (? . boy) likes girl (? . girl))
    )
   )
 )
)

; Frames: empty frame.
(assert-test empty-frame frame?)

; Frames: single value.
(define a-frame (frame-bind empty-frame 'a 1))
(assert-equal? '((a 1)) (frame->list a-frame))
(assert-equal? '(a . 1) (frame-get a-frame 'a))

; Frames: found nothing.
(assert-equal? '() (frame-get a-frame 'b))

; Frames: two values.
(define ab-frame (frame-bind a-frame 'b 2))
(assert-equal? '((b 2) (a 1)) (frame->list ab-frame))
(assert-equal? '(a . 1) (frame-get ab-frame 'a))
(assert-equal? '(b . 2) (frame-get ab-frame 'b))
(assert-equal? '() (frame-get ab-frame 'c))
