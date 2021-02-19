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

; Instantiate: scalar variables.
(assert-equal? '(assert (1) + (2) = (3))
 (instantiate
  '(assert (? . a) + (? . b) = (? . c))
  (make-frame '((a 1) (b 2) (c 3)))
 )
)

; Instantiate: list variables.
(assert-equal? '(assert (1 2) + (3 4) = (1 2 3 4))
 (instantiate
  '(assert (? . a) + (? . b) = (? . c))
  (make-frame '((a 1 2) (b 3 4) (c 1 2 3 4)))
 )
)

(define (deps? var exp . bindings)
 (exp-depends-on?
  (parse-query exp)
  (cons '? var)
  (make-frame (map parse-query bindings))
 )
)

; Depends on: same variable.
(assert-true? (deps? 'x '?x ))
(assert-false? (deps? 'y '?x ))

; Depends on: complex expression.
(
 (lambda (exp)
  (assert-true? (deps? 'x exp))
  (assert-true? (deps? 'y exp))
  (assert-false? (deps? 'z exp))
 )
 '(and (person ?y) (not (busy ?x)))
)

; Depends on: expression in variable.
(assert-true?
 (deps?
  'x
  '(and (person ?y) (not (busy ?z)))
  '(y (match ?v yes))
  '(z (match ?x true))
 )
)

; Rename vars: expression w/o vars.
(assert-equal?
 '(and (person Mike) (not? synth))
 (rename-vars-in
  next-unique-var-id
  '(and (person Mike) (not? synth))
 )
)

; Rename vars: expression with vars.
(assert-equal?
 '(and (person (? . $1:person)) (not? (? . $1:type)))
 (rename-vars-in
  next-unique-var-id
  '(and (person (? . person)) (not? (? . type)))
 )
)

; Resolve variables: no variables.
(assert-equal? '((c . X) (b 1 2) (a . 1))
 (resolve-all-vars '((a . 1) (b . (1 2)) (c . X)))
)

; Resolve variables: single post-reference.
(assert-equal? '((c . X) (a . 1) (b . 1))
 (resolve-all-vars '((a . 1) (b . (? . a)) (c . X)))
)

; Resolve variables: single pre-reference.
(assert-equal? '((c . X) (a . 1) (b . X))
 (resolve-all-vars '((a . 1) (b . (? . c)) (c . X)))
)

; Resolve variables: nested references.
(assert-equal? '((a . X) (b . X) (c . X))
 (resolve-all-vars '((a . (? . c)) (b . X) (c . (? . b))))
)
