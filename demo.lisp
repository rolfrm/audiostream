;; The intention to make something like this possible. (not there yet)
(use-package :live-audio)
(set-tempo 120)
(defvar bass (plong -1))
(defvar line1 (select-scale 'a 'minor))

(defvar seq (scale-seq 0 0 1 0 2 0 3 0))

(defvar sequencer (make-sequencer))

(sequencer-start sequencer seq :line line1 :instrument bass)
