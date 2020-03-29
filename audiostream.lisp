
;;;; audiostream.lisp

(in-package #:audiostream)

(defvar *buffer-size* 1024)

(defun process-buffers(get-next-buffer)
  (let ((pa (pa-new)))
    (loop while t do
	 (let ((buffer (funcall get-next-buffer)))
	   (pa-write pa buffer)
	   ))))

(defun get-buffer-processor(fill-buffer)
  (let ((semaphore (sb-thread:make-semaphore))
	(process-semaphore (sb-thread:make-semaphore :count 2))
	(buffers nil)
	(ready-buf nil)
	)
    (flet ((get-buffer ()
	     (sb-thread:wait-on-semaphore semaphore)
	     (let ((buf buffers))
	       (loop until (eq (cdr buf) ready-buf) do
		    (setf buf (cdr buf)))
	       (setf ready-buf buf)
	       (setf (cdr buf) nil)
	       (sb-thread:signal-semaphore process-semaphore)
	       (car buf)
	       )
	     )
	   (proc-buf ()
	     (sb-thread:wait-on-semaphore process-semaphore)
	     (let ((buf (make-array *buffer-size* :element-type 'single-float)))
	       (funcall fill-buffer buf)
	       (setf buffers (cons buf buffers))
	       (sb-thread:signal-semaphore semaphore)
	       )))
      (list #'get-buffer #'proc-buf)

      )))

(defvar *generators* ())

(defvar *gen-lock* (sb-thread:make-mutex))

(defun push-gen (gen)
  (sb-thread:with-mutex (*gen-lock*)
    (setf *generators* (cons gen *generators*))))

(defparameter *base-vol* 0.5)

(defun pling(freq)
  (let ((phase 0.0)
	(volume 1.0))
    (flet ((sine ()
	     (incf phase (/ (* 2 pi) 44100.0))
	     (setf volume (* 0.9999 volume))
	     (if (< volume 0.0001)
		 nil
		 (* volume *base-vol* (sin (* freq phase))))))
      #'sine)))

(defun push-pling(freq)
  (push-gen (let ((phase 0.0)
	       (volume 1.0))
	   (flet ((sine ()
		    (incf phase (/ (* 2 pi) 44100.0))
		    (setf volume (* 0.9999 volume))
		    (if (< volume 0.0001)
			nil
			(* volume *base-vol* (sin (* freq phase))))))
		  #'sine))))


(defun sleep-samples (time)
  (let ((sem (sb-thread:make-semaphore)))
    (push-gen (flet ((delay ()
			   (decf time (/ 1.0 44100.0))
			   (if (> time 0)
			       0.0
			       (progn
				 (sb-thread:signal-semaphore sem)
				 nil))))
		    
		    #'delay))
    (sb-thread:wait-on-semaphore sem :timeout (* time 5))
    ))
    
(defun --test()
  (defparameter *sleep-time* 0.25)
  (loop for i from 0 below 5 do
       (progn
	 ;;(push-pling 1000)
	 (push-pling 440)
	 (push-pling 666)
	 (time (sleep-samples 1))
	 ))
  (progn
    (loop for i from 0 below 2 do
	 (progn
	   (push-pling 2000.0)
	   (sleep-samples *sleep-time*)
	   (push-pling 1000)
	   (sleep-samples *sleep-time*)

	   ))
    (loop for i from 0 below 2 do
	 (progn
	   (push-pling 2200.0)
	   (sleep-samples *sleep-time*)
	   (push-pling 800)
	   (sleep-samples *sleep-time*)

	   ))
    )


  (setf *generators* nil)
  )
(defun calc-next-sample()
  (let ((sample 0.0))
    (labels ((-calc-next-sample (gen)
	       (when gen
		 (let ((v (funcall (car gen))))
		   (if v
		       (progn
			 (incf sample v)
			 (setf (cdr gen) (-calc-next-sample (cdr gen)))
			 gen)

		       (-calc-next-sample (cdr gen)))))))
      (setf *generators* (-calc-next-sample *generators*)))
    sample))

(defstruct audio
  (processing t)
  (generators nil)
  )

(defun audio-start()
  (let ((audio (make-audio))
	(pa (pa-new))
	(generators nil))
    (flet ((calc-next-sample2 ()
	     (let ((sample 0.0))
	       (labels ((-calc-next-sample (gen)
			  (when gen
			    (let ((v (funcall (car gen))))
			      (if v
				  (progn
				    (incf sample v)
				    (setf (cdr gen) (-calc-next-sample (cdr gen)))
				    gen)
				  
				  (-calc-next-sample (cdr gen)))))))
		 (setf generators (-calc-next-sample generators)))
	       sample)))    
    (flet ((audio-gen (buffer)
	     (let ((generators2 (audio-generators audio)))
	       (when generators2
		 (when (eq (sb-ext:compare-and-swap (audio-generators audio) generators2 nil) generators2)
		   (setf generators (concatenate 'list generators2 (audio-generators audio))))))
	     (dotimes (i (array-total-size buffer))
	       (setf (aref buffer i) (coerce  (/ (atan (calc-next-sample2)) 0.5 pi) 'single-float)))))
      (destructuring-bind (get-buffer proc-buf) (get-buffer-processor #'audio-gen)
	(flet (
	       (play-buffers ()
		 (loop while (audio-processing audio) do
		      (let ((buffer (funcall get-buffer)))
			(pa-write pa buffer)))
		 (pa-free pa)
		 (print "Stopping audio..")
		 )
	       (get-samples ()
		 (loop while (audio-processing audio) do
		      (funcall proc-buf)))
	       )
	  (sb-thread:make-thread #'play-buffers)
	  (sb-thread:make-thread #'get-samples)
	  )))
    audio
    )))

(defun audio-add-gen(audio generator)
  (setf (audio-generators audio) (cons generator (audio-generators audio))))

(defun audio-stop(audio)
  (setf (audio-processing audio) nil))

(defun test-proc-buffer()

  (flet ((sinegen (buffer)
	   (sb-thread:with-mutex (*gen-lock*)
	     (dotimes (i (array-total-size buffer))
	       (setf (aref buffer i) (coerce  (/ (atan (calc-next-sample)) 0.5 pi) 'single-float))))))
    (destructuring-bind (get-buffer proc-buf) (get-buffer-processor #'sinegen)
      (flet (
	     (proc ()
	       (loop while t do
		    (process-buffers get-buffer))))
	(sb-thread:make-thread #'proc)
	(loop while t do
	     (funcall proc-buf))))))

(defun test-proc-buffer2()
  (let ((phase 0)
	(frequency 440))
    (flet ((sinegen (buffer)
	     (dotimes (i (array-total-size buffer))
	       (setf (aref buffer i) (* 0.2 (sin (* frequency phase))))
	       (incf phase (/ 1.0 44100)))))
      
      (destructuring-bind (get-buffer proc-buf) (get-buffer-processor #'sinegen)
	(dotimes (i 5)
	  (funcall proc-buf)
	  (print (funcall get-buffer)))))))
    
	
