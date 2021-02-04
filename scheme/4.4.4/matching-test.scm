(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "defs.scm")
(include "utilities.scm")
(include "matching.scm")

(define (match query statement)
 (define frame (pattern-match (parse-query query) statement empty-frame))
 (if (eq? void frame) 'null (frame->list frame))
)

(define (match-frame bindings query statement)
 (define frame (pattern-match (parse-query query) statement (make-frame bindings)))
 (if (eq? void frame) 'null (frame->list frame))
)

(define (umatch a b)
 (define frame (unify-match (parse-query a) (parse-query b) empty-frame))
 (if (eq? void frame) 'null (frame->list frame))
)

(define (urmatch a b)
 (define frame (unify-match-resolved (parse-query a) (parse-query b) empty-frame))
 (if (eq? void frame) 'null (frame->list frame))
)

(define (umatch-frame bindings a b)
 (define frame
  (unify-match
   (parse-query a)
   (parse-query b)
   (make-frame (map parse-query bindings))
  )
 )

 (if (eq? void frame) 'null (frame->list frame))
)


; Main tests for match.
(define (test-match match)
 ; Matching: dummy queries without variables.
 (assert-equal? '() (match '(a) '(a)))
 (assert-equal? '() (match '(a b) '(a b)))
 (assert-equal? '() (match '(a b c) '(a b c)))
 (assert-equal? '() (match '(a (b)) '(a (b))))
 (assert-equal? '() (match '(a (b) c) '(a (b) c)))
 (assert-equal? '() (match '(a (b c)) '(a (b c))))
 (assert-equal? '() (match '(a (b c) d) '(a (b c) d)))

 ; Matching: not matching dummy queries without variables.
 (assert-equal? 'null (match '(a) '(b)))
 (assert-equal? 'null (match '(a) '(a b)))
 (assert-equal? 'null (match '(a c) '(a b)))
 (assert-equal? 'null (match '(a b c) '(a b d)))
 (assert-equal? 'null (match '(a (b) c) '(a b c)))
 (assert-equal? 'null (match '(a (b c) d) '(a (b d) d)))

 ; Matching: single variable of plain value.
 (assert-equal? '((a a)) (match '(?a) '(a)))
 (assert-equal? '((a a)) (match '(?a b) '(a b)))
 (assert-equal? '((b b)) (match '(a ?b) '(a b)))
 (assert-equal? '((b b)) (match '(a ?b c) '(a b c)))

 ; Matching: multiple variables of plain values.
 (assert-equal? '((b b) (a a)) (match '(?a ?b) '(a b)))
 (assert-equal? '((c c) (b b) (a a)) (match '(?a ?b ?c) '(a b c)))
 (assert-equal? '((b b) (a a)) (match '(x ?a ?b y) '(x a b y)))

 ; Matching: variables of list values.
 (assert-equal? '((ab (a b))) (match '(?ab) '((a b))))
 (assert-equal? '((ab (a b)) (x x)) (match '(?x ?ab) '(x (a b))))
 (assert-equal? '((cde (c d e)) (ab (a b))) (match '(?ab ?cde) '((a b) (c d e))))
 (assert-equal? '((de (d e)) (c (c)) (ab (a b))) (match '(?ab ?c ?de) '((a b) (c) (d e))))

 ; Matching: nested variables.
 (assert-equal? '((a a)) (match '((?a)) '((a))))
 (assert-equal? '((b b) (a a)) (match '((?a (?b))) '((a (b)))))

 ; Matching: matching constistency.
 (assert-equal? '((x (a b))) (match '(?x c ?x) '((a b) c (a b))))
 (assert-equal? 'null (match '(?x c ?x) '((a b) b (a b))))
 (assert-equal? 'null (match '(?x c ?x) '((a b) c (a d))))

 ; Matching: dot notation.
 (assert-equal? '((b (b)) (a a)) (match '(?a . ?b) '(a b)))
 (assert-equal? '((cd (c d)) (a a)) (match '(?a b . ?cd) '(a b c d)))
 (assert-equal? '((cd (c d)) (b b) (a a)) (match '(?a (?b . ?cd)) '(a (b c d))))
)

; Tests for match with frames.
(define (test-match-with-frames match-frame)
 ; Matching: single frame on the input.
 (assert-equal? '((b b) (c c)) (match-frame '((c . c)) '(a ?b ?c) '(a b c)))

 ; Matching: wrong value of the frame on the input.
 (assert-equal? 'null (match-frame '((c . x)) '(a ?b ?c) '(a b c)))
)

; Run tests for statements match:
(test-match match)
(test-match-with-frames match-frame)

; Run tests for unified match:
(test-match umatch)
(test-match (lambda (b a) (umatch a b)))
(test-match-with-frames umatch-frame)

; Unify match: cross-matching with samples from SICP.
(assert-equal? '((y a) (z a) (x a))
 (urmatch '(?x a ?y) '(?y ?z a))
)

(assert-equal? 'null
 (urmatch '(?x ?y a) '(?x b ?y))
)

(assert-equal? '((z c) (y b) (x (a b c)))
 (urmatch '(?x ?x) '((a ?y c) (a b ?z)))
)

(assert-equal? '((x (b (? . y))) (z a))
 (urmatch '(?x a) '((b ?y) ?z))
)
