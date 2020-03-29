(defpackage :live-audio
  (:use :cl :audiostream)
  )

(in-package :live-audio)

(defparameter *tempo* 160)

(defun set-tempo(tempo)
  (setf *tempo* tempo))


(defparameter *audio* (audio-start))
(defstruct beat
  (tempo 0)
  (process t))

(defun start-beat(on-beat &optional (mul 1))
  (let ((tempo (/ *tempo* 60.0))
	(time 0.0)
	(beat (make-beat))
	) 
    (flet ((process-sequence ()
	     (incf time (/ tempo 44100.0))
	     (when (> time mul)
	       (decf time mul)
	       (funcall on-beat))
	     (if (beat-process beat)
	       
		 0.0
		 (progn
		   (print 'beat)
		   nil)
		 )
	     ))
      (audio-add-gen *audio* #'process-sequence))
    beat))

(defparameter *beat* (start-beat (lambda ()  (audio-add-gen *audio* (audiostream::pling 80)))))
(setf (beat-process *beat*) nil)

(defun piano-frequency (note)
  (* 440 (expt 2 (/ (- note 49) 12))))

(defparameter *major-scale* #(0 2 4 5 7 9 11))
(defparameter *minor-scale* #(0 2 3 5 7 9 10))
(defun scale-select (note scale)
  (let ((len (length scale)))
    (let ((idx (mod note len))
	  (octave (floor (/ note len))))
      (+ 49 (* 12 octave) (aref scale idx)))))

(defun melody(&rest song)
  (let ((pos nil))
    (flet ((play()
	     (unless pos (setf pos song))
	     (audio-add-gen *audio* (audiostream::pling (piano-frequency (car pos))))
	     (setf pos (cdr pos))))
      #'play)))
	
(defparameter *beat2* (start-beat (apply #'melody (loop for i in '(0 5 0 2 4 0 3 7 0 5 0 2 4 0 3 5) collect (print (scale-select (+ i 3) *major-scale*)))) 0.5))
;(setf (beat-process *beat2*) nil)
(defparameter *sequences* nil)

(defstruct mseq
  (notes)
  (position 0)
  (repeat nil)
  (instrument nil)
  (time 1)
  )

(defparameter *tick* 0)
(defparameter *music* (make-hash-table))
(defun sub-beat()
  (incf *tick*)

  (loop for x in *sequences* do
       (let* ((p (mseq-position x))
	      (nodes (mseq-notes x))
	      (p2 (mod (+ p (mseq-time x)) (length nodes)))
	      (note (when (integerp p) (aref nodes p))))
	 (setf (mseq-position x) p2)

	 (when note
	   (let ((instr (mseq-instrument x)))
	     (unless instr
	       (setf instr (lambda (freq) (audiostream::pling freq))))

	     (when (keywordp instr)
	       (setf instr (gethash instr *music*)))

	     (if (functionp note)
		 (funcall note)
		 (audio-add-gen *audio* (funcall instr (piano-frequency note))))))
       )))

(defun on-beat()
  (handler-case (sub-beat)

    (condition (c) (print c))
    (serious-condition (c) (print c))
    
  ))

(when nil
  (setf *sequences* (cons (make-mseq :notes #(49 40 53 42) :position 0 :repeat t)
			(cons (make-mseq :notes #(45 37 50 37) :position 0 :repeat t) *sequences*)))

  (setf *sequences* (cons (make-mseq :notes #(25 25 27 25) :position 0 :repeat t) *sequences*))
  (progn
  (setf *sequences* (cons (make-mseq :notes #(20 0 18 0 20 20 0 0) :position 0 :repeat t) nil))
					;(setf *sequences* (cons (make-mseq :notes #(0 0 0 0 40 40 40 40) :position 0 :repeat t) *sequences*))
  ;(setf *sequences* (cons (make-mseq :notes #(0 0 40 40 40 40 0 0) :position 0 :repeat t) *sequences*))
  (setf *sequences* (cons (make-mseq :notes #(90 90 80 80 90 90 0 0) :position 0 :repeat t) *sequences*))
  ;(setf *sequences* (cons (make-mseq :notes #(0 0 0 50 0 50 0 0) :position 0 :repeat t) *sequences*))
  )

  (setf *sequences* (cons (make-mseq :notes #(0 0 0 0 90 90 0 0) :position 0 :repeat t) *sequences*))
  (setf *sequences* (cons (make-mseq :notes #(0 0 0 0 0 85 85 0) :position 0 :repeat t) *sequences*))
  (setf *sequences* (cons (make-mseq :notes #(0 0 0 0 0 100 0 100) :position 0 :repeat t) *sequences*))
 (setf *seqences* nil))
(defparameter *beat* (start-beat (lambda () (on-beat)) 0.25))
;(setf (beat-process *beat*) nil)
(audio-stop *audio*)
(setf *audio* (audio-start))



(defun play(key &rest args)
  (let* ((repeat (getf args :repeat))
	 (notes (when repeat (make-array (length repeat) :initial-contents repeat)))
	 (scale (getf args :scale))
	 (instr (getf args :instrument))
	 (time (getf args :time 1))
	 (current (gethash key *music*)))
    (when (and scale notes)
      (dotimes (i (array-total-size notes))
	(when (integerp (aref notes i))
	  (setf (aref notes i) (scale-select (aref notes i) scale)))))

    (unless current
      (setf (gethash key *music*) (setf current (make-mseq :notes notes :repeat t))))
    (setf (mseq-instrument current) instr)
    (setf (mseq-notes current) notes)
    (setf (mseq-time current) time)
    (unless (integerp (/ (mseq-position current) time))
      (setf (mseq-position current) 0))
    (unless (find current *sequences*)
      (setf *sequences* (cons current *sequences*)))
    
    ))


(defun pling(freq &optional (sustain 0.9999))
  (let ((phase 0.0)
	(volume 1.0))
    (flet ((sine ()
	     (incf phase (/ (* 2 pi) 44100.0))
	     (setf volume (* sustain volume))
	     (decf sustain (/ 1.0 44100.0))
	     (when (> sustain 0.0001)
		 nil
		 (* volume 1.0 (sin (* freq phase))))))
      #'sine)))

(defun adsr (attack decay sustain release)
  (setf attack (* attack 2 pi))
  (setf decay (* decay 2 pi))
  (setf sustain (* sustain 2 pi))
  (setf release (* release 2 pi))
  (let* ((ad (+ attack decay))
	(ads (+ ad sustain))
	(asdr (+ ads release)))
    (flet ((env (phase)
	     (cond
	       ((< phase attack) (/ phase attack))
	       ((< phase ad) (- 1 (* 0.3 (/ (- phase attack) decay))))
	       ((< phase ads) 0.7)
	       ((< phase asdr) (* 0.7 (max 0 (- 1 (/ (- phase ads) release 0.9)))))
	       (t nil))))
      #'env)))


(defun instrument (key &rest args)
  (setf (gethash key *music*) (car args)))

(defun pling-instrument(sustain)
  (lambda (freq) (pling (* 0.25 freq) sustain)))

(defun square(phase)
  (if (> (mod phase pi) 1)
      1.0
      -1.0))

(defun bass2-gen(phase)
  (* (square phase) (+ 1.0 (* 0.25 (sin (* 0.1 phase))))))

(defun bass2-instrument(a d s r)
  (let ((env (adsr a d s r)))
    (lambda (freq)
      (let ((phase 0.0)
	    (prevsamp (bass2-gen 0.0)))
	(lambda ()
	  (let ((r (funcall env phase))
		(samp (bass2-gen (* freq phase 2))))
	    (when r
	      (setf r (* 0.25 r prevsamp))
	      (incf phase (/ 1.0 44100)))
	    (setf prevsamp (+ (* 0.1 samp) (* 0.9 prevsamp)))
	    r))))))


(instrument :bass1 (bass2-instrument 0.01 0.01 0.01 0.01))

(instrument :bass2 (bass2-instrument 0.01 0.01 0.2 0.01))
(play :bline1 :time 1/2 :instrument  :bass1 :scale *minor-scale* :repeat '(1 3 4 3 5 7 8 7
