(include "tree-test-base.scm")


(add 10)
(log-sample "initial tree")

(add 5 3 7)
(log-sample "added 5 3 7")

(add 1 2 6)
(log-sample "added 1 2 6")

(add 15 13 12 10)
(log-sample "added 15 13 12 10")

(add 0 14 11)
(log-sample "added 0 14 11")

(delete 11 6 0 7)
(log-sample "deleted leafs 11 6 0 7")

(delete 1 3 15)
(log-sample "deleted singles 1 3 15")

(add 1 0 11 15 17 4 3)
(log-sample "added 1 0 11 15 17 4 3")

(delete 13)
(log-sample "deleted double 13")

(delete 2)
(log-sample "deleted double 2")

(delete 10)
(log-sample "deleted double root 10")
