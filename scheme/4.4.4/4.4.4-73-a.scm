;
; Test «A» demonstrates standard implementation of streams
; flatten with delayed recursion. It prints iteration over
; infinite integer triples (triple a b c) where a + b = c.
;
; The implementation in «4.4.4-73-test.scm» involves
; special forms «set» and «amb» that make QEval to be
; more like programming language (over streams).
;
; Some details. With «or» form we able to multiply
; the streams. As «or» uses delayed interleaving,
; we are not able to make truly infinite stream
; that may be consumed at once (hung) by a rule
; when flattening is not delayed.
;
; To demonstrate the issue we have to make truly
; infinite stream that works well with iteration
; over delayed flattening.
;
(include "../4.4.4/qeval-test-core.scm")
(include "4.4.4-73-test.scm")

(log-iter 16 (query-iter (triple ?a ?b ?c)))
;
; 00:  (triple 1 1 2)
; 01:  (triple 1 2 3)
; 02:  (triple 1 3 4)
; 03:  (triple 1 4 5)
; 04:  (triple 2 2 4)
; 05:  (triple 1 5 6)
; 06:  (triple 2 3 5)
; 07:  (triple 1 6 7)
; 08:  (triple 2 4 6)
; 09:  (triple 1 7 8)
; 10:  (triple 3 3 6)
; 11:  (triple 2 5 7)
; 12:  (triple 1 8 9)
; 13:  (triple 3 4 7)
; 14:  (triple 2 6 8)
; 15:  (triple 1 9 10)
;
