(include "2.1.4-2.x.scm")

(log "interval 1 to 2 is " (interval-str (interval-make 1 2)))

(log "[1 .. 2] + [-1 .. 0.5] = " (interval-str
  (interval-add (interval-make 1 2) (interval-make -1 0.5))))

(log "[1 .. 2] * [1 .. 1.5] = " (interval-str
  (interval-mul (interval-make 1 2) (interval-make 1 1.5))))

(log "[1 .. 2] * [-1 .. 1] = " (interval-str
  (interval-mul (interval-make 1 2) (interval-make -1 1))))

(log "inverse [0.1 .. 2] = " (interval-str
  (interval-inverse (interval-make 0.1 2))))

(log "[0.1 .. 2] / [0.1 .. 2] = " (interval-str
  (interval-div (interval-make 0.1 2) (interval-make 0.1 2))))
