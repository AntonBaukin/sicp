(include "2.5.3-rat-uni.scm")

; Use special version of rational package that applies
; unified operations as it's required in §2.5.3-2.93.
(define rational-package-init
 install-rational-uni-package
)

(include "2.5.3-base.scm")
(include "2.5.3-rat-cut-int.scm")

(install-arithmetic-package
 'rational-cut-int
 install-rational-cut-int-package
)

